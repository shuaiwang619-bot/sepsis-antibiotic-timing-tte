/* =====================================================================================
   抗生素启动时机的目标试验仿真 (TTE) + 克隆-删失-加权 (CCW) —— 完整可复现 SQL
   ---------------------------------------------------------------------------------
   作者:   [作者]
   日期:   2026-06-12
   版本:   formal_v1, corrected boundary + corrected baseline SOFA direction + audit fields
   数据源: MIMIC-IV (PhysioNet)
   工作区: schema  abx_tte_formal_v1  (本研究全部数据/中间表都在这里,与其他项目隔离)
   ---------------------------------------------------------------------------------
   本文件功能:
     从零开始,只依赖 mimiciv_* 源 schema,跑完一次得到完整数据管线:
       (1) 主队列 (含暴露分窗 + CCW 角色)        ->  abx_tte_formal_v1.cohort_exposure (24,735)
       (2) 基线协变量 (人口学+合并症+基线 SOFA)   ->  abx_tte_formal_v1.cov_base        (24,735)
       (3) 时变协变量 (0-24h 逐小时)              ->  abx_tte_formal_v1.cov_tvcov       (618,375)
       (4) 结局 (30天死亡+器官恶化+LOS)           ->  abx_tte_formal_v1.outcomes        (24,735)
   ---------------------------------------------------------------------------------
   核心定稿决策(所有"为什么这么定"的说明都在每一段的注释里):
     - 正式 v1 输出写入 abx_tte_formal_v1 schema, 不覆盖旧 abx_tte 探索版
      - t0 (time zero) = culture_time, culture-only 主分析
     - culture 取 sofa_time ±72h 窗口内、按 hadm 匹配的最早培养
     - t0 清洗:不晚于出ICU、不早于入ICU 24h
     - 排晚发脓毒症:t0 不晚于入ICU + 7天
     - 抗生素:严格 IV (剔除 NULL route 与 INJECTION)
     - 暴露分窗:t0 后 72h 内首剂 -> 0-3/3-6/6-12/>=12h
     - SOFA 基线:三档回填 (t0前 -> t0后6h -> ICU入科后24h),解决 t0<ICU入科 的缺失
     - 休克:基于 mimiciv_derived.sofa 的 rate 列 (norepi/epi/dopa>0),不计 dobutamine
     - 时变协变量:逐小时 LOCF (向前结转最近24h内的最新观测值)
     - 30天死亡:右删失到 t0+30d, 出院不删失, 事件优先, dod按日级
     - 器官恶化主口径:t0后72h SOFA 峰值较基线升高 >= 2
   ---------------------------------------------------------------------------------
   期望核对数 (Step 末尾都有验证查询):
     step1_first_icu             ~ 54,551
     step2a_first_sofa           ~ 26,425
     cohort (清洗+排晚发)         = 24,735
     cohort_exposure 七桶合计      = 24,735
       analyzable                = 18,167  (四个时机臂之和)
       censor_noinit             =    786  (no_iv_in_72h)
       excluded_pretreated       =  5,782  (pre_t0 + pre_t0_earlier_only)
     cov_base / outcomes 行数     = 24,735
     cov_tvcov 行数               = 618,375 (= 24,735 × 25 小时)
     30天死亡率                   ~ 19.7%
     30天死亡内院内占比           ~ 79.4%
   ===================================================================================== */



/* =====================================================================================
   FORMAL_V1 修正说明 (2026-06-12)
   ---------------------------------------------------------------------------------
   1) 暴露边界追平清理后桌面基线与 CCW: [0,3] / (3,6] / (6,12] / (12,72h)。
   2) 基线 SOFA 三档排序修正: tier1 取 t0 前最近; tier2/3 取 t0 后最早, 且保留 6h/24h 上限。
   3) cov_tvcov 写入 still_observed, 用于风险集缺失率与时序建模审计。
   4) 器官恶化的 sofa_peak_72h 改为直接从 mimiciv_derived.sofa 抽 t0 后 72h 峰值。
   5) 输出 schema 改为 abx_tte_formal_v1, 旧 abx_tte 仅作探索版存档。
   ===================================================================================== */

/* =====================================================================================
   ── STEP 0 ── 工作区 + 全局参数
   ---------------------------------------------------------------------------------
   决策: 全部数据存入独立 schema `abx_tte`, 与其他项目和 public 中间表彻底隔离;
         所有时窗/阈值参数集中在 cfg 表,改一处全局生效,便于敏感性分析。
   ===================================================================================== */

CREATE SCHEMA IF NOT EXISTS abx_tte_formal_v1;

DROP TABLE IF EXISTS abx_tte_formal_v1.cfg;
CREATE TABLE abx_tte_formal_v1.cfg AS
SELECT
  -- 队列定义
  INTERVAL '24 hours' AS min_icu_los,              -- ICU 最短停留时间(短于此剔除,避免极短ICU 干扰)
  INTERVAL '24 hours' AS min_t0_before_icu,        -- t0 允许早于入ICU 的最大距离(超过即剔除,排除社区/远ED采样)
  INTERVAL '7 days'   AS max_t0_after_icu,         -- 排晚发: t0 不晚于入ICU+此 (限定为社区/入院早期脓毒症)

  -- 培养-SOFA 配对窗
  INTERVAL '72 hours' AS culture_vs_sofa_window,   -- culture 必须落在 sofa_time ±此窗口内(否则视为不是同一发病)

  -- 抗生素暴露窗
  INTERVAL '72 hours' AS pre_t0_window,            -- pretreated 判定:t0 前此窗口内有 IV = 主分析排除
  INTERVAL '72 hours' AS post_t0_window,           -- t0 后首剂搜索上限

  -- SOFA 基线取值规则
  INTERVAL '24 hours' AS sofa_lookup_pre,          -- tier1: t0 前此窗内最近一条 SOFA
  INTERVAL '6 hours'  AS sofa_lookup_post,         -- tier2: t0 后此窗内最早一条 SOFA (兜底)
  INTERVAL '24 hours' AS sofa_icu_fallback,        -- tier3: t0<ICU入科时,用入ICU后此窗的首条 SOFA(回补)

  -- 时变网格
  24 AS tvcov_hours;                               -- 时变协变量逐小时网格上限(t0 到 t0+24h, 25 行)


/* =====================================================================================
   ── STEP 1 ── 首次 ICU 入科 (subject 级去重,成人,ICU >= 24h)
   ---------------------------------------------------------------------------------
   决策:
     - 按 subject 取首次 ICU (一个人只入一次研究队列,避免相关样本)
     - 成人 = 入ICU时点年龄 >= 18(用 anchor_age + 入院年-anchor_year 校正,比直接用 anchor_age 严)
     - ICU LOS >= 24h (短于此停留不能稳定建立基线/时变协变量)

   为何不用 icustays.first_icu_stay = 1?
     该字段是"该次 hadm 内的首个 ICU",同一 subject 多次住院会有多条 =1,
     会让同一病人多次进队列。用 ROW_NUMBER 按 subject 排序更准。

   预期: ~54,551
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step1_first_icu;
CREATE TABLE abx_tte_formal_v1.step1_first_icu AS
WITH ranked AS (
  SELECT
    i.subject_id, i.hadm_id, i.stay_id,
    i.intime  AS icu_intime,
    i.outtime AS icu_outtime,
    p.gender, p.dod,
    (p.anchor_age + (EXTRACT(YEAR FROM i.intime)::int - p.anchor_year))::int AS age_at_icu,
    ROW_NUMBER() OVER (PARTITION BY i.subject_id ORDER BY i.intime, i.stay_id) AS rn
  FROM mimiciv_icu.icustays i
  JOIN mimiciv_hosp.patients p ON p.subject_id = i.subject_id
  WHERE i.intime IS NOT NULL
    AND i.outtime IS NOT NULL
    AND (i.outtime - i.intime) >= (SELECT min_icu_los FROM abx_tte_formal_v1.cfg)
)
SELECT subject_id, hadm_id, stay_id, icu_intime, icu_outtime, gender, dod, age_at_icu
FROM ranked
WHERE rn = 1
  AND age_at_icu >= 18;

CREATE INDEX ix_s1_subject ON abx_tte_formal_v1.step1_first_icu (subject_id);
CREATE INDEX ix_s1_hadm    ON abx_tte_formal_v1.step1_first_icu (hadm_id);
CREATE INDEX ix_s1_stay    ON abx_tte_formal_v1.step1_first_icu (stay_id);

-- 校验: 总数 = distinct subject (每人一条) ≈ 54,551
SELECT COUNT(*) AS step1_n,
       COUNT(DISTINCT subject_id) AS n_subjects
FROM abx_tte_formal_v1.step1_first_icu;


/* =====================================================================================
   ── STEP 2a ── 每个 stay 的首次 sofa_time (Sepsis-3 阳性)
   ---------------------------------------------------------------------------------
   决策: 用 mimiciv_derived.sepsis3 派生表的 sepsis3=true 行,
         每 stay 取最早的 sofa_time 作为"脓毒症发作"参考时点。

   注意: sofa_time 是 SOFA 评分提示脓毒症的时点,本身不直接作为 t0(t0 用 culture),
         它只用来把 culture 限定到"和脓毒症相关"的时窗里。

   预期: ~26,425
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step2a_first_sofa;
CREATE TABLE abx_tte_formal_v1.step2a_first_sofa AS
SELECT
  f.subject_id, f.hadm_id, f.stay_id,
  f.icu_intime, f.icu_outtime, f.gender, f.dod, f.age_at_icu,
  sf.sofa_time
FROM abx_tte_formal_v1.step1_first_icu f
JOIN (
  SELECT stay_id, MIN(sofa_time) AS sofa_time
  FROM mimiciv_derived.sepsis3
  WHERE sepsis3 = true AND sofa_time IS NOT NULL
  GROUP BY stay_id
) sf ON sf.stay_id = f.stay_id;

CREATE INDEX ix_s2a_stay ON abx_tte_formal_v1.step2a_first_sofa (stay_id);
CREATE INDEX ix_s2a_hadm ON abx_tte_formal_v1.step2a_first_sofa (hadm_id);

-- 校验: ≈ 26,425
SELECT COUNT(*) AS step2a_n FROM abx_tte_formal_v1.step2a_first_sofa;


/* =====================================================================================
   ── STEP 2b ── 在 sofa_time ±72h 内、按 hadm 匹配的最早培养时间
   ---------------------------------------------------------------------------------
   决策:
     - culture 来自 mimiciv_derived.suspicion_of_infection (suspected_infection=1)
     - 在 sofa_time ±72h 窗口内
     - 按 hadm_id (整次住院) 匹配,因为培养常在 ED/病房抽,早于 ICU stay_id 存在
     - 取窗内最早 culture,作为后续 t0 候选

   为何用 hadm_id 而不是 stay_id?
     stay_id 只覆盖 ICU 期间;ED/病房采的培养在 stay_id 之前,只能靠 hadm 匹配。
     这一步直接决定了 ~半数患者 t0 早于 ICU 入科 (median t0-intime = -1h)。

   为何 ±72h?
     既反映"感染先于器官衰竭"的临床现实(culture 偏早),
     也允许"先有器官衰竭后才做培养"(culture 偏晚)。
     ±72h 在文献中是常用宽窗,敏感性可缩到 ±48h/±24h。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step2b_culture;
CREATE TABLE abx_tte_formal_v1.step2b_culture AS
SELECT
  s.subject_id, s.hadm_id, s.stay_id,
  s.icu_intime, s.icu_outtime, s.gender, s.dod, s.age_at_icu,
  s.sofa_time,
  MIN(soi.culture_time) AS culture_time
FROM abx_tte_formal_v1.step2a_first_sofa s
CROSS JOIN abx_tte_formal_v1.cfg cfg
LEFT JOIN mimiciv_derived.suspicion_of_infection soi
       ON soi.subject_id = s.subject_id
      AND soi.hadm_id    = s.hadm_id
      AND soi.suspected_infection = 1
      AND soi.culture_time IS NOT NULL
      AND soi.culture_time >= s.sofa_time - cfg.culture_vs_sofa_window
      AND soi.culture_time <= s.sofa_time + cfg.culture_vs_sofa_window
GROUP BY s.subject_id, s.hadm_id, s.stay_id,
         s.icu_intime, s.icu_outtime, s.gender, s.dod, s.age_at_icu, s.sofa_time;

CREATE INDEX ix_s2b_stay ON abx_tte_formal_v1.step2b_culture (stay_id);


/* =====================================================================================
   ── STEP 2c ── 主队列冻结: t0 = culture_time + 清洗 + 排晚发
   ---------------------------------------------------------------------------------
   t0 定义决策 (culture-only, 主分析):
     最早尝试过 t0 = MAX(culture, sofa_time),但导致 ~43% 患者 t0 晚于
     真实首剂(pre_treated 比例过高,排除后样本严重缩水)。
     改用 culture-only 后 pre_treated 降到 ~21%,且 t0 更贴近真实临床决策。
     max() 版本保留为敏感性分析。

   清洗规则:
     (a) culture_time <= icu_outtime
         (t0 不能晚于出ICU,否则进入"恢复期",不属于研究问题)
     (b) culture_time >= icu_intime - 24h
         (t0 早于入ICU 超过 24h 的患者,可能是 ED 长时间逗留或社区脓毒症,
          基线/时变协变量大量缺失,先排除;敏感性可放宽到 24h 外)

   排晚发脓毒症:
     (c) culture_time <= icu_intime + 7天
         (t0 远晚于入ICU 的患者属于"ICU 获得性/迟发脓毒症",
          感染机制和抗生素决策逻辑不同,主分析剔除,仅 243 例(0.97%),
          作为入排标准直接排除,光明正大写进 Methods,比"敏感性"更利落)

   预期最终主队列 = 24,735, 三个 bad 计数均 = 0
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.cohort;
CREATE TABLE abx_tte_formal_v1.cohort AS
SELECT
  b.subject_id, b.hadm_id, b.stay_id,
  b.icu_intime, b.icu_outtime, b.gender, b.dod, b.age_at_icu,
  b.sofa_time,
  b.culture_time AS sepsis_t0
FROM abx_tte_formal_v1.step2b_culture b
CROSS JOIN abx_tte_formal_v1.cfg cfg
WHERE b.culture_time IS NOT NULL                                  -- 必须配到培养
  AND b.culture_time <= b.icu_outtime                             -- 清洗 (a)
  AND b.culture_time >= b.icu_intime - cfg.min_t0_before_icu      -- 清洗 (b)
  AND b.culture_time <= b.icu_intime + cfg.max_t0_after_icu;      -- 排晚发 (c)

CREATE INDEX ix_coh_subject ON abx_tte_formal_v1.cohort (subject_id);
CREATE INDEX ix_coh_hadm    ON abx_tte_formal_v1.cohort (hadm_id);
CREATE INDEX ix_coh_stay    ON abx_tte_formal_v1.cohort (stay_id);
CREATE INDEX ix_coh_t0      ON abx_tte_formal_v1.cohort (sepsis_t0);

-- 校验: cohort_n = 24,735; 三个 bad 计数 = 0
SELECT
  COUNT(*) AS cohort_n,
  COUNT(DISTINCT subject_id) AS n_subjects,
  COUNT(*) FILTER (WHERE sepsis_t0 > icu_outtime) AS bad_t0_after_out,
  COUNT(*) FILTER (WHERE sepsis_t0 < icu_intime - INTERVAL '24 hours') AS bad_t0_early,
  COUNT(*) FILTER (WHERE sepsis_t0 > icu_intime + INTERVAL '7 days') AS bad_late_onset
FROM abx_tte_formal_v1.cohort;


/* =====================================================================================
   ── STEP 3a ── 严格 IV 抗生素事件 (按 hadm,覆盖 ED + 病房 + ICU)
   ---------------------------------------------------------------------------------
   决策:
     - 用 mimiciv_derived.antibiotic 派生表 (覆盖 ED+病房+ICU 的所有抗生素)
     - 严格 IV 口径: route LIKE 'IV%' OR LIKE '%INTRAVENOUS%'
     - 剔除 NULL route (无法判定途径,保守剔除)
     - 剔除 INJECTION (肌注/皮下/其他注射,语义不是"静脉启动")
     - 按 hadm_id 关联 (因为很多首剂在 ED/病房,早于 stay_id)

   为何不放宽 (比如纳入口服/不限 route)?
     研究问题是"静脉抗生素启动时机",口服与目标性升级不属于"经验性 IV 启动"决策,
     宽口径会把 PO 误算成"启动",虚低延迟。宽口径作为敏感性分析。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step3a_iv_abx;
CREATE TABLE abx_tte_formal_v1.step3a_iv_abx AS
SELECT DISTINCT
  a.subject_id, a.hadm_id, a.starttime AS abx_time
FROM mimiciv_derived.antibiotic a
JOIN abx_tte_formal_v1.cohort c
     ON a.subject_id = c.subject_id AND a.hadm_id = c.hadm_id
WHERE a.starttime IS NOT NULL
  AND a.route IS NOT NULL                                    -- 严格: 剔除 NULL
  AND ( UPPER(a.route) LIKE 'IV%' OR UPPER(a.route) LIKE '%INTRAVENOUS%' )
  AND UPPER(a.route) NOT LIKE '%INJECTION%';                 -- 严格: 剔除注射类

CREATE INDEX ix_s3a ON abx_tte_formal_v1.step3a_iv_abx (subject_id, hadm_id, abx_time);


/* =====================================================================================
   ── STEP 3b ── 暴露分窗 + CCW 角色三档 (最终冻结主队列)
   ---------------------------------------------------------------------------------
   分窗决策:
     pre_t0                 = t0 前 72h 内已有 IV  -> 主分析排除 (无法分配启动策略)
     pre_t0_earlier_only    = 仅有更早 (>72h前) 的 IV -> 主分析排除 (非同决策时段)
     no_iv_in_72h_after_t0  = t0 后 72h 内未启动 IV  -> 留队列,CCW 中作删失(非剔除)
     0-3h / 3-6h / 6-12h    = 可比四臂中的三个早窗; 正式 v1 边界为 [0,3] / (3,6] / (6,12]
     >=12h                  = 第四臂(晚启动)

   ccw_role 三档 (决定 CCW 中如何处理):
     analyzable         = 进克隆、按时机分臂比较的对象
     censor_noinit      = 留在风险集,在合适时点被 IPCW 删失(不剔除)
     excluded_pretreated = 不进 CCW 主分析,但保留以便 CONSORT 流程图
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.cohort_exposure;
CREATE TABLE abx_tte_formal_v1.cohort_exposure AS
WITH agg AS (
  SELECT
    c.subject_id, c.hadm_id, c.stay_id,
    c.icu_intime, c.icu_outtime, c.gender, c.dod, c.age_at_icu,
    c.sofa_time, c.sepsis_t0,
    MIN(a.abx_time) FILTER (
      WHERE a.abx_time >= c.sepsis_t0 - INTERVAL '72 hours' AND a.abx_time < c.sepsis_t0
    ) AS first_abx_pre_72h,
    MIN(a.abx_time) FILTER (
      WHERE a.abx_time < c.sepsis_t0 - INTERVAL '72 hours'
    ) AS first_abx_pre_earlier,
    MIN(a.abx_time) FILTER (
      WHERE a.abx_time >= c.sepsis_t0 AND a.abx_time < c.sepsis_t0 + INTERVAL '72 hours'
    ) AS first_abx_post_72h
  FROM abx_tte_formal_v1.cohort c
  LEFT JOIN abx_tte_formal_v1.step3a_iv_abx a
         ON a.subject_id = c.subject_id AND a.hadm_id = c.hadm_id
  GROUP BY c.subject_id, c.hadm_id, c.stay_id,
           c.icu_intime, c.icu_outtime, c.gender, c.dod, c.age_at_icu,
           c.sofa_time, c.sepsis_t0
)
SELECT
  *,
  CASE WHEN first_abx_post_72h IS NULL THEN NULL
       ELSE EXTRACT(EPOCH FROM (first_abx_post_72h - sepsis_t0)) / 3600.0
  END AS abx_delay_hours,
  CASE
    WHEN first_abx_pre_72h IS NOT NULL THEN 'pre_t0'
    WHEN first_abx_pre_72h IS NULL AND first_abx_pre_earlier IS NOT NULL THEN 'pre_t0_earlier_only'
    WHEN first_abx_post_72h IS NULL THEN 'no_iv_in_72h_after_t0'
    WHEN first_abx_post_72h <= sepsis_t0 + INTERVAL '3 hours' THEN '0-3h'
    WHEN first_abx_post_72h <= sepsis_t0 + INTERVAL '6 hours' THEN '3-6h'
    WHEN first_abx_post_72h <= sepsis_t0 + INTERVAL '12 hours' THEN '6-12h'
    ELSE '>=12h'
  END AS abx_window,
  CASE
    WHEN first_abx_pre_72h IS NOT NULL
      OR (first_abx_pre_72h IS NULL AND first_abx_pre_earlier IS NOT NULL)
      THEN 'excluded_pretreated'
    WHEN first_abx_post_72h IS NULL THEN 'censor_noinit'
    ELSE 'analyzable'
  END AS ccw_role
FROM agg;

CREATE INDEX ix_ce_subject ON abx_tte_formal_v1.cohort_exposure (subject_id);
CREATE INDEX ix_ce_stay    ON abx_tte_formal_v1.cohort_exposure (stay_id);
CREATE INDEX ix_ce_window  ON abx_tte_formal_v1.cohort_exposure (abx_window);
CREATE INDEX ix_ce_role    ON abx_tte_formal_v1.cohort_exposure (ccw_role);

-- 校验: 七桶合计 = 24,735; 三档合计 = 24,735; analyzable ≈ 18,167
SELECT abx_window, COUNT(*) AS n FROM abx_tte_formal_v1.cohort_exposure
GROUP BY abx_window
ORDER BY CASE abx_window
  WHEN 'pre_t0' THEN 1 WHEN 'pre_t0_earlier_only' THEN 2
  WHEN '0-3h' THEN 3 WHEN '3-6h' THEN 4 WHEN '6-12h' THEN 5
  WHEN '>=12h' THEN 6 WHEN 'no_iv_in_72h_after_t0' THEN 7 ELSE 9 END;

SELECT ccw_role, COUNT(*) FROM abx_tte_formal_v1.cohort_exposure GROUP BY ccw_role ORDER BY ccw_role;



/* FORMAL_V1 HARD CHECKS after exposure classification.
   These are stop rules: if any count drifts, do not continue the formal run.
*/
DO $$
DECLARE
  bad_n int;
BEGIN
  WITH expected AS (
    SELECT '0-3h'::text AS abx_window, 3439::bigint AS expected_n
    UNION ALL SELECT '3-6h', 3241
    UNION ALL SELECT '6-12h', 5017
    UNION ALL SELECT '>=12h', 6470
  ),
  observed AS (
    SELECT abx_window, COUNT(*)::bigint AS observed_n
    FROM abx_tte_formal_v1.cohort_exposure
    WHERE ccw_role = 'analyzable'
    GROUP BY abx_window
  )
  SELECT COUNT(*) INTO bad_n
  FROM expected e
  LEFT JOIN observed o ON o.abx_window = e.abx_window
  WHERE COALESCE(o.observed_n, -1) <> e.expected_n;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 hard check failed: analyzable window counts must be 3439/3241/5017/6470.';
  END IF;
END $$;

DO $$
DECLARE
  bad_n int;
BEGIN
  WITH expected AS (
    SELECT 'analyzable'::text AS ccw_role, 18167::bigint AS expected_n
    UNION ALL SELECT 'censor_noinit', 786
    UNION ALL SELECT 'excluded_pretreated', 5782
  ),
  observed AS (
    SELECT ccw_role, COUNT(*)::bigint AS observed_n
    FROM abx_tte_formal_v1.cohort_exposure
    GROUP BY ccw_role
  )
  SELECT COUNT(*) INTO bad_n
  FROM expected e
  LEFT JOIN observed o ON o.ccw_role = e.ccw_role
  WHERE COALESCE(o.observed_n, -1) <> e.expected_n;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 hard check failed: ccw_role counts must be analyzable=18167, censor_noinit=786, excluded_pretreated=5782.';
  END IF;
END $$;

DO $$
DECLARE
  bad_n int;
BEGIN
  WITH observed AS (
    SELECT
      COUNT(*) FILTER (WHERE abx_delay_hours = 3 AND abx_window = '0-3h'  AND ccw_role = 'analyzable') AS delay3_ok,
      COUNT(*) FILTER (WHERE abx_delay_hours = 6 AND abx_window = '3-6h'  AND ccw_role = 'analyzable') AS delay6_ok,
      COUNT(*) FILTER (WHERE abx_delay_hours = 12 AND abx_window = '6-12h' AND ccw_role = 'analyzable') AS delay12_ok
    FROM abx_tte_formal_v1.cohort_exposure
  )
  SELECT CASE WHEN delay3_ok = 106 AND delay6_ok = 136 AND delay12_ok = 69 THEN 0 ELSE 1 END
  INTO bad_n
  FROM observed;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 hard check failed: boundary rows must be delay=3 -> 0-3h (106), delay=6 -> 3-6h (136), delay=12 -> 6-12h (69).';
  END IF;
END $$;

/* =====================================================================================
   ── STEP 4a ── Charlson 合并症 (按 hadm)
   ---------------------------------------------------------------------------------
   直接从 mimiciv_derived.charlson 取分项(每个 hadm 一行),
   作为基线混杂直接进入 outcome 模型。

   局限性说明:
     Charlson 基于整次住院的诊断码,而诊断码是出院时编码的 ->
     严格说含了 t0 之后才确诊的病(MIMIC 做合并症的通病,Methods 里说明即可)。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step4a_charlson;
CREATE TABLE abx_tte_formal_v1.step4a_charlson AS
SELECT
  ch.hadm_id, ch.subject_id,
  ch.charlson_comorbidity_index, ch.age_score,
  ch.myocardial_infarct, ch.congestive_heart_failure,
  ch.peripheral_vascular_disease, ch.cerebrovascular_disease,
  ch.dementia, ch.chronic_pulmonary_disease,
  ch.rheumatic_disease, ch.peptic_ulcer_disease,
  ch.mild_liver_disease, ch.severe_liver_disease,
  ch.diabetes_without_cc, ch.diabetes_with_cc,
  ch.paraplegia, ch.renal_disease,
  ch.malignant_cancer, ch.metastatic_solid_tumor, ch.aids
FROM mimiciv_derived.charlson ch
WHERE ch.hadm_id IS NOT NULL;

CREATE INDEX ix_s4a_hadm ON abx_tte_formal_v1.step4a_charlson (hadm_id);


/* =====================================================================================
   ── STEP 4b ── 基线 SOFA + 休克 (三档回填策略)
   ---------------------------------------------------------------------------------
   核心难点:
     约 50% 患者 t0 早于入ICU入科 (median t0-intime = -1h),
     而 mimiciv_derived.sofa 只在 ICU 内有数据 -> 这批人 t0 时点根本没 SOFA。
     直接"只向前取"会有 ~66% 缺失,基线严重度大面积空白,IPCW 调不动混杂。

   三档回填决策 (按优先级,只取第一档命中):
     tier 1: SOFA endtime ∈ [t0 - 24h, t0]      (主取法: 直接拿 t0 前最近一条)
     tier 2: SOFA endtime ∈ (t0, t0 + 6h]       (兜底: t0 后短时间内的早期值)
     tier 3: 仅 t0 < icu_intime 时启用,
             SOFA endtime ∈ [icu_intime, icu_intime + 24h]
             (回补: t0 早于入科者,取入ICU后 24h 内首条 SOFA 当基线,
              因为 t0-intime 的 p90 < 13h,回补值离 t0 不远,偏倚很小)

   sofa_source_flag:
     = 1: tier 1 或 tier 2 命中 (原生)
     = 0: tier 3 命中 (回补) -> 用于敏感性: 仅用 flag=1 的人复跑

   休克 (shock_t0_B) 来自同一行的速率列:
     rate_norepinephrine, rate_epinephrine, rate_dopamine 任一 > 0 -> 休克
     dobutamine 是强心药不计入休克 (Methods 注明)
     vasopressin/phenylephrine 该 sofa 表没有,作为敏感性可补 inputevents

   经此三档回填,SOFA 缺失从 66% 降到 0.08% (20/24735),近全量可用。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.step4b_sofa_base;

/* Optimized formal_v1 STEP 4B.
   Same clinical rule, faster set-based implementation:
   - tier1: latest SOFA before or at t0
   - tier2: earliest SOFA after t0 within 6h
   - tier3: if t0 precedes ICU intime, earliest SOFA within ICU first 24h
*/
CREATE TABLE abx_tte_formal_v1.step4b_sofa_base AS
WITH base AS (
  SELECT
    m.stay_id, m.subject_id, m.hadm_id, m.sepsis_t0, m.icu_intime,
    cfg.sofa_lookup_pre,
    cfg.sofa_lookup_post,
    cfg.sofa_icu_fallback
  FROM abx_tte_formal_v1.cohort_exposure m
  CROSS JOIN abx_tte_formal_v1.cfg cfg
),
candidates AS (
  SELECT
    b.stay_id, b.subject_id, b.hadm_id, b.sepsis_t0, b.icu_intime,
    s.endtime AS sofa_time_t0,

    s.sofa_24hours           AS sofa_total_t0,
    s.respiration_24hours    AS respiration_24hours_t0,
    s.coagulation_24hours    AS coagulation_24hours_t0,
    s.liver_24hours          AS liver_24hours_t0,
    s.cardiovascular_24hours AS cardiovascular_24hours_t0,
    s.cns_24hours            AS cns_24hours_t0,
    s.renal_24hours          AS renal_24hours_t0,

    s.meanbp_min             AS meanbp_min_t0,
    s.gcs_min                AS gcs_min_t0,
    s.uo_24hr                AS uo_24hr_t0,

    (
      COALESCE(s.rate_norepinephrine, 0) > 0
      OR COALESCE(s.rate_epinephrine, 0) > 0
      OR COALESCE(s.rate_dopamine, 0) > 0
    ) AS shock_t0_b,

    CASE
      WHEN s.endtime >= b.sepsis_t0 - b.sofa_lookup_pre
       AND s.endtime <= b.sepsis_t0
        THEN 1
      WHEN s.endtime > b.sepsis_t0
       AND s.endtime <= b.sepsis_t0 + b.sofa_lookup_post
        THEN 2
      WHEN b.sepsis_t0 < b.icu_intime
       AND s.endtime >= b.icu_intime
       AND s.endtime <= b.icu_intime + b.sofa_icu_fallback
        THEN 3
      ELSE 9
    END AS sofa_tier
  FROM base b
  JOIN mimiciv_derived.sofa s
    ON s.stay_id = b.stay_id
   AND (
        (
          s.endtime >= b.sepsis_t0 - b.sofa_lookup_pre
          AND s.endtime <= b.sepsis_t0 + b.sofa_lookup_post
        )
        OR
        (
          b.sepsis_t0 < b.icu_intime
          AND s.endtime >= b.icu_intime
          AND s.endtime <= b.icu_intime + b.sofa_icu_fallback
        )
   )
),
picked AS (
  SELECT DISTINCT ON (stay_id)
    *,
    CASE WHEN sofa_tier IN (1, 2) THEN 1 ELSE 0 END AS sofa_source_flag
  FROM candidates
  WHERE sofa_tier IN (1, 2, 3)
  ORDER BY
    stay_id,
    CASE sofa_tier WHEN 1 THEN 1 WHEN 2 THEN 2 WHEN 3 THEN 3 ELSE 9 END,
    CASE WHEN sofa_tier = 1 THEN sofa_time_t0 END DESC NULLS LAST,
    CASE WHEN sofa_tier IN (2, 3) THEN sofa_time_t0 END ASC NULLS LAST
)
SELECT
  m.stay_id,
  m.subject_id,
  m.hadm_id,
  m.sepsis_t0,
  m.icu_intime,

  p.sofa_total_t0,
  p.respiration_24hours_t0,
  p.coagulation_24hours_t0,
  p.liver_24hours_t0,
  p.cardiovascular_24hours_t0,
  p.cns_24hours_t0,
  p.renal_24hours_t0,

  p.meanbp_min_t0,
  p.gcs_min_t0,
  p.uo_24hr_t0,

  COALESCE(p.shock_t0_b, FALSE) AS shock_t0_b,
  p.sofa_source_flag
FROM abx_tte_formal_v1.cohort_exposure m
LEFT JOIN picked p
  ON p.stay_id = m.stay_id;

CREATE INDEX ix_s4b_stay ON abx_tte_formal_v1.step4b_sofa_base (stay_id);

-- 校验: 行数 = 24,735; sofa_total_t0 缺失约 20; formal_v1 mean SOFA 约 2.694
SELECT COUNT(*) AS n,
       COUNT(*) FILTER (WHERE sofa_total_t0 IS NULL) AS n_sofa_missing,
       COUNT(*) FILTER (WHERE sofa_source_flag = 0) AS n_icu_fallback,
       COUNT(*) FILTER (WHERE shock_t0_b) AS n_shock_t0,
       ROUND(AVG(sofa_total_t0)::numeric, 3) AS mean_sofa_total_t0
FROM abx_tte_formal_v1.step4b_sofa_base;


/* =====================================================================================
   ── STEP 4c ── 基线协变量主表 (cov_base)
   ---------------------------------------------------------------------------------
   主队列 + 人口学(admissions) + Charlson + 基线 SOFA + 休克 全部合并。

   主模型用变量(缺失率 < 2%): 人口学、Charlson 各分量、基线 SOFA 总分/分量、shock_t0_b。
   敏感性/扩展(缺失率高): 乳酸/pH/BE/胆红素/肌酐/血小板 等 (不在主表,见 cov_tvcov 的乳酸)。

   weekend_admission: 入ICU是周末(周六/日),为流程延迟混杂(免费且零成本的变量)。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.cov_base;
CREATE TABLE abx_tte_formal_v1.cov_base AS
SELECT
  m.stay_id, m.subject_id, m.hadm_id,
  m.icu_intime, m.icu_outtime, m.sepsis_t0,
  m.age_at_icu, m.gender,
  a.race, a.admission_type, a.admission_location, a.discharge_location,
  (EXTRACT(ISODOW FROM m.icu_intime) IN (6,7))::int AS weekend_admission,

  -- Charlson 合并症
  ch.charlson_comorbidity_index, ch.age_score,
  ch.myocardial_infarct, ch.congestive_heart_failure,
  ch.peripheral_vascular_disease, ch.cerebrovascular_disease,
  ch.dementia, ch.chronic_pulmonary_disease,
  ch.rheumatic_disease, ch.peptic_ulcer_disease,
  ch.mild_liver_disease, ch.severe_liver_disease,
  ch.diabetes_without_cc, ch.diabetes_with_cc,
  ch.paraplegia, ch.renal_disease,
  ch.malignant_cancer, ch.metastatic_solid_tumor, ch.aids,

  -- 基线 SOFA + 休克
  b.sofa_total_t0,
  b.respiration_24hours_t0, b.coagulation_24hours_t0, b.liver_24hours_t0,
  b.cardiovascular_24hours_t0, b.cns_24hours_t0, b.renal_24hours_t0,
  b.meanbp_min_t0, b.gcs_min_t0, b.uo_24hr_t0,
  b.shock_t0_b,
  b.sofa_source_flag,             -- 1=原生 / 0=ICU回补 (敏感性用)

  -- 暴露信息冗余写入(便于直接 JOIN cov_base 就能拿到分窗)
  m.abx_window, m.ccw_role,
  m.first_abx_pre_72h, m.first_abx_pre_earlier, m.first_abx_post_72h,
  m.abx_delay_hours
FROM abx_tte_formal_v1.cohort_exposure m
LEFT JOIN mimiciv_hosp.admissions a ON a.hadm_id = m.hadm_id
LEFT JOIN abx_tte_formal_v1.step4a_charlson  ch ON ch.hadm_id = m.hadm_id AND ch.subject_id = m.subject_id
LEFT JOIN abx_tte_formal_v1.step4b_sofa_base  b ON b.stay_id = m.stay_id;

CREATE INDEX ix_covb_stay  ON abx_tte_formal_v1.cov_base (stay_id);
CREATE INDEX ix_covb_role  ON abx_tte_formal_v1.cov_base (ccw_role);

-- 校验: 行数 = 24,735; sofa_total_t0 缺失 ≈ 20 (0.08%)
SELECT COUNT(*) AS n,
       COUNT(*) FILTER (WHERE sofa_total_t0 IS NULL) AS n_sofa_missing,
       COUNT(*) FILTER (WHERE sofa_source_flag = 0)  AS n_icu_fallback,
       COUNT(*) FILTER (WHERE shock_t0_b)            AS n_shock_t0
FROM abx_tte_formal_v1.cov_base;


/* =====================================================================================
   ── STEP 5 ── 时变协变量 (cov_tvcov, 0-24h 逐小时, LOCF)
   ---------------------------------------------------------------------------------
   用途: 仅用于 IPCW (删失模型/加权),不直接进 outcome 主模型 (避免过度调整)。

   网格: t0 到 t0+24h,逐小时(25 行/人),共 24735 × 25 = 618,375 行。

   LOCF 规则: 每个小时点的值 = "该时点之前最近一条观测",但**只在 t0 前 24h 内回溯**
              (避免使用过于久远的、与当前时点无关的观测)。

   字段:
     SOFA 动态        (sofa 表逐小时,取每个 grid_time 之前最近一条)
     生理量           (meanbp_min, gcs_min, uo_24hr - 来自 sofa 表的窗内最差值)
     休克             (shock_tvcov_b: 同 sofa 表的 rate 列,>0 即为在用升压药)
     乳酸             (labevents 中 ILIKE '%LACTATE%')
     机械通气         (mimiciv_derived.ventilation 区间覆盖 grid_time 即为 on)
     抗生素事件       (abx_started_by_hour: 该小时窗内是否首剂启动)
     缺失标志         (sofa_tvcov_missing, shock_tvcov_missing, lactate_tvcov_missing)
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.cov_tvcov_ccw_model_input;
DROP TABLE IF EXISTS abx_tte_formal_v1.cov_tvcov;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_grid;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_sofa_src;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_sofa_pick;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_vent_src;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_vent_pick;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_lactate_src;
DROP TABLE IF EXISTS abx_tte_formal_v1.step5_lactate_pick;

/* Optimized formal_v1 STEP 5.
   Purpose: create raw 0-24h time-varying covariate grid.
   Performance rule: prefilter source tables once, index them, then use DISTINCT ON.
*/

/* 5.1 hourly grid */
CREATE TABLE abx_tte_formal_v1.step5_grid AS
SELECT
  m.stay_id,
  m.subject_id,
  m.hadm_id,
  m.sepsis_t0,
  m.icu_outtime,
  m.dod,
  g.t_hour::int AS t_hour,
  m.sepsis_t0 + make_interval(hours => g.t_hour::int) AS grid_time,
  g.t_hour::numeric AS time_from_t0_h,
  m.first_abx_post_72h,
  m.abx_delay_hours
FROM abx_tte_formal_v1.cohort_exposure m
CROSS JOIN generate_series(0, 24) AS g(t_hour);

CREATE INDEX ix_s5_grid_stay_hour ON abx_tte_formal_v1.step5_grid (stay_id, t_hour);
CREATE INDEX ix_s5_grid_stay_time ON abx_tte_formal_v1.step5_grid (stay_id, grid_time);

/* 5.2 SOFA source, restricted to relevant stays and t0 +/- 24h */
CREATE TABLE abx_tte_formal_v1.step5_sofa_src AS
SELECT
  s.stay_id,
  s.endtime,
  s.sofa_24hours,
  s.respiration_24hours,
  s.coagulation_24hours,
  s.liver_24hours,
  s.cardiovascular_24hours,
  s.cns_24hours,
  s.renal_24hours,
  s.meanbp_min,
  s.gcs_min,
  s.uo_24hr,
  s.rate_norepinephrine,
  s.rate_epinephrine,
  s.rate_dopamine
FROM mimiciv_derived.sofa s
JOIN abx_tte_formal_v1.cohort_exposure m
  ON m.stay_id = s.stay_id
WHERE s.endtime >  m.sepsis_t0 - INTERVAL '24 hours'
  AND s.endtime <= m.sepsis_t0 + INTERVAL '24 hours';

CREATE INDEX ix_s5_sofa_src_stay_time ON abx_tte_formal_v1.step5_sofa_src (stay_id, endtime);

/* 5.3 SOFA LOCF pick */
CREATE TABLE abx_tte_formal_v1.step5_sofa_pick AS
SELECT DISTINCT ON (g.stay_id, g.t_hour)
  g.stay_id,
  g.t_hour,
  s.sofa_24hours,
  s.respiration_24hours,
  s.coagulation_24hours,
  s.liver_24hours,
  s.cardiovascular_24hours,
  s.cns_24hours,
  s.renal_24hours,
  s.meanbp_min,
  s.gcs_min,
  s.uo_24hr,
  s.rate_norepinephrine,
  s.rate_epinephrine,
  s.rate_dopamine
FROM abx_tte_formal_v1.step5_grid g
LEFT JOIN abx_tte_formal_v1.step5_sofa_src s
  ON s.stay_id = g.stay_id
 AND s.endtime <= g.grid_time
 AND s.endtime >  g.sepsis_t0 - INTERVAL '24 hours'
ORDER BY
  g.stay_id,
  g.t_hour,
  s.endtime DESC NULLS LAST;

CREATE INDEX ix_s5_sofa_pick_stay_hour ON abx_tte_formal_v1.step5_sofa_pick (stay_id, t_hour);

/* 5.4 ventilation source */
CREATE TABLE abx_tte_formal_v1.step5_vent_src AS
SELECT
  v.stay_id,
  v.starttime,
  v.endtime,
  v.ventilation_status
FROM mimiciv_derived.ventilation v
JOIN abx_tte_formal_v1.cohort_exposure m
  ON m.stay_id = v.stay_id
WHERE v.starttime <= m.sepsis_t0 + INTERVAL '24 hours'
  AND (v.endtime IS NULL OR v.endtime >= m.sepsis_t0);

CREATE INDEX ix_s5_vent_src_stay_start_end ON abx_tte_formal_v1.step5_vent_src (stay_id, starttime, endtime);

/* 5.5 ventilation pick */
CREATE TABLE abx_tte_formal_v1.step5_vent_pick AS
SELECT DISTINCT ON (g.stay_id, g.t_hour)
  g.stay_id,
  g.t_hour,
  v.ventilation_status
FROM abx_tte_formal_v1.step5_grid g
LEFT JOIN abx_tte_formal_v1.step5_vent_src v
  ON v.stay_id = g.stay_id
 AND v.starttime <= g.grid_time
 AND (v.endtime IS NULL OR v.endtime > g.grid_time)
ORDER BY
  g.stay_id,
  g.t_hour,
  v.starttime DESC NULLS LAST;

CREATE INDEX ix_s5_vent_pick_stay_hour ON abx_tte_formal_v1.step5_vent_pick (stay_id, t_hour);

/* 5.6 lactate source.
   Lactate is sensitivity-only; source is still materialized for audit and later sensitivity models.
*/
CREATE TABLE abx_tte_formal_v1.step5_lactate_src AS
SELECT
  lc.hadm_id,
  lc.subject_id,
  lc.charttime,
  lc.valuenum
FROM mimiciv_hosp.labevents lc
JOIN mimiciv_hosp.d_labitems di
  ON di.itemid = lc.itemid
JOIN (
  SELECT DISTINCT subject_id, hadm_id, sepsis_t0
  FROM abx_tte_formal_v1.cohort_exposure
) m
  ON m.subject_id = lc.subject_id
 AND m.hadm_id = lc.hadm_id
WHERE lc.valuenum IS NOT NULL
  AND di.label ILIKE '%LACTATE%'
  AND lc.charttime >  m.sepsis_t0 - INTERVAL '24 hours'
  AND lc.charttime <= m.sepsis_t0 + INTERVAL '24 hours';

CREATE INDEX ix_s5_lactate_src_key_time ON abx_tte_formal_v1.step5_lactate_src (subject_id, hadm_id, charttime);

/* 5.7 lactate LOCF pick */
CREATE TABLE abx_tte_formal_v1.step5_lactate_pick AS
SELECT DISTINCT ON (g.stay_id, g.t_hour)
  g.stay_id,
  g.t_hour,
  lc.valuenum AS lactate_tvcov
FROM abx_tte_formal_v1.step5_grid g
LEFT JOIN abx_tte_formal_v1.step5_lactate_src lc
  ON lc.subject_id = g.subject_id
 AND lc.hadm_id = g.hadm_id
 AND lc.charttime <= g.grid_time
 AND lc.charttime >  g.sepsis_t0 - INTERVAL '24 hours'
ORDER BY
  g.stay_id,
  g.t_hour,
  lc.charttime DESC NULLS LAST;

CREATE INDEX ix_s5_lactate_pick_stay_hour ON abx_tte_formal_v1.step5_lactate_pick (stay_id, t_hour);

/* 5.8 final raw cov_tvcov */
CREATE TABLE abx_tte_formal_v1.cov_tvcov AS
SELECT
  g.stay_id,
  g.subject_id,
  g.hadm_id,
  g.sepsis_t0,
  g.t_hour,
  g.grid_time,
  g.time_from_t0_h,

  CASE WHEN g.grid_time <= LEAST(
              g.icu_outtime,
              COALESCE(g.dod::timestamp + INTERVAL '23 hours 59 minutes 59 seconds',
                       g.sepsis_t0 + INTERVAL '24 hours'),
              g.sepsis_t0 + INTERVAL '24 hours'
            )
       THEN 1 ELSE 0 END AS still_observed,

  sf.sofa_24hours           AS sofa_total_tvcov,
  sf.respiration_24hours    AS respiration_24hours_tvcov,
  sf.coagulation_24hours    AS coagulation_24hours_tvcov,
  sf.liver_24hours          AS liver_24hours_tvcov,
  sf.cardiovascular_24hours AS cardiovascular_24hours_tvcov,
  sf.cns_24hours            AS cns_24hours_tvcov,
  sf.renal_24hours          AS renal_24hours_tvcov,
  sf.meanbp_min             AS meanbp_min_tvcov,
  sf.gcs_min                AS gcs_min_tvcov,
  sf.uo_24hr                AS uo_24hr_tvcov,

  (
    COALESCE(sf.rate_norepinephrine, 0) > 0
    OR COALESCE(sf.rate_epinephrine, 0) > 0
    OR COALESCE(sf.rate_dopamine, 0) > 0
  ) AS shock_tvcov_b,

  lc.lactate_tvcov,
  COALESCE(v.ventilation_status = 'on', FALSE) AS vent_tvcov,

  CASE WHEN g.first_abx_post_72h IS NOT NULL
        AND g.first_abx_post_72h >= g.grid_time
        AND g.first_abx_post_72h <  g.grid_time + INTERVAL '1 hour'
       THEN 1 ELSE 0 END AS abx_started_by_hour,

  CASE WHEN g.first_abx_post_72h IS NULL OR g.first_abx_post_72h >= g.grid_time
       THEN 1 ELSE 0 END AS at_risk_for_initiation,

  g.abx_delay_hours    AS first_abx_delay_hours,
  g.first_abx_post_72h AS first_abx_time,

  (sf.sofa_24hours IS NULL) AS sofa_tvcov_missing,
  (
    sf.rate_norepinephrine IS NULL
    AND sf.rate_epinephrine IS NULL
    AND sf.rate_dopamine IS NULL
  ) AS shock_tvcov_missing,
  (lc.lactate_tvcov IS NULL) AS lactate_tvcov_missing
FROM abx_tte_formal_v1.step5_grid g
LEFT JOIN abx_tte_formal_v1.step5_sofa_pick sf
  ON sf.stay_id = g.stay_id
 AND sf.t_hour = g.t_hour
LEFT JOIN abx_tte_formal_v1.step5_vent_pick v
  ON v.stay_id = g.stay_id
 AND v.t_hour = g.t_hour
LEFT JOIN abx_tte_formal_v1.step5_lactate_pick lc
  ON lc.stay_id = g.stay_id
 AND lc.t_hour = g.t_hour
ORDER BY g.stay_id, g.t_hour;

CREATE INDEX ix_tv_stayhour ON abx_tte_formal_v1.cov_tvcov (stay_id, t_hour);
CREATE INDEX ix_tv_grid ON abx_tte_formal_v1.cov_tvcov (stay_id, grid_time);
CREATE INDEX ix_tv_risk ON abx_tte_formal_v1.cov_tvcov (still_observed);

/* 5.9 raw cov_tvcov audit */
SELECT
  COUNT(*) AS n_rows,
  COUNT(DISTINCT stay_id) AS n_stays,
  COUNT(*) FILTER (WHERE still_observed = 1) AS n_risk_rows,
  COUNT(*) FILTER (WHERE still_observed = 1 AND sofa_total_tvcov IS NULL) AS n_sofa_missing_in_risk,
  COUNT(*) FILTER (WHERE still_observed = 1 AND meanbp_min_tvcov IS NULL) AS n_meanbp_missing_in_risk,
  COUNT(*) FILTER (WHERE still_observed = 1 AND lactate_tvcov IS NULL) AS n_lactate_missing_in_risk
FROM abx_tte_formal_v1.cov_tvcov;

/* =====================================================================================
   STEP 5B - CCW modeling time-varying input
   ---------------------------------------------------------------------------------
   Purpose: apply agreed modeling cleanup after raw cov_tvcov generation.
   Rules:
   - Missingness audits use still_observed = 1 only.
   - sofa_total_tvcov and meanbp_min_tvcov are anchored at t_hour=0 using formal baseline values, then LOCF forward only.
   - shock_tvcov_b is anchored at t_hour=0 using formal baseline shock, then LOCF forward only.
   - Six SOFA components remain extension-only and are not force-filled in the main model table.
   - lactate remains sensitivity-only; no main-model fill except missing indicators.
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.cov_tvcov_ccw_model_input;

CREATE TABLE abx_tte_formal_v1.cov_tvcov_ccw_model_input AS
WITH x AS (
  SELECT
    t.*,
    b.sofa_total_t0,
    b.meanbp_min_t0,
    b.shock_t0_b AS shock_t0_b_base,

    CASE
      WHEN t.t_hour = 0 AND t.sofa_total_tvcov IS NULL THEN b.sofa_total_t0
      ELSE t.sofa_total_tvcov
    END AS sofa_total_seed,

    CASE
      WHEN t.t_hour = 0 AND t.meanbp_min_tvcov IS NULL THEN b.meanbp_min_t0
      ELSE t.meanbp_min_tvcov
    END AS meanbp_seed,

    CASE
      WHEN t.t_hour = 0 AND t.shock_tvcov_b IS NULL THEN b.shock_t0_b
      ELSE t.shock_tvcov_b
    END AS shock_seed
  FROM abx_tte_formal_v1.cov_tvcov t
  LEFT JOIN abx_tte_formal_v1.cov_base b
    ON b.stay_id = t.stay_id
),
grp AS (
  SELECT
    *,
    COUNT(sofa_total_seed) OVER (PARTITION BY stay_id ORDER BY t_hour) AS sofa_grp,
    COUNT(meanbp_seed)     OVER (PARTITION BY stay_id ORDER BY t_hour) AS meanbp_grp,
    COUNT(shock_seed)      OVER (PARTITION BY stay_id ORDER BY t_hour) AS shock_grp
  FROM x
)
SELECT
  stay_id,
  subject_id,
  hadm_id,
  sepsis_t0,
  t_hour,
  grid_time,
  time_from_t0_h,
  still_observed,

  sofa_total_tvcov AS sofa_total_tvcov_raw,
  meanbp_min_tvcov AS meanbp_min_tvcov_raw,
  shock_tvcov_b AS shock_tvcov_b_raw,

  MAX(sofa_total_seed) OVER (PARTITION BY stay_id, sofa_grp) AS sofa_total_tvcov,
  respiration_24hours_tvcov,
  coagulation_24hours_tvcov,
  liver_24hours_tvcov,
  cardiovascular_24hours_tvcov,
  cns_24hours_tvcov,
  renal_24hours_tvcov,
  MAX(meanbp_seed) OVER (PARTITION BY stay_id, meanbp_grp) AS meanbp_min_tvcov,
  gcs_min_tvcov,
  uo_24hr_tvcov,
  (MAX(shock_seed::int) OVER (PARTITION BY stay_id, shock_grp))::boolean AS shock_tvcov_b,

  lactate_tvcov,
  vent_tvcov,
  abx_started_by_hour,
  at_risk_for_initiation,
  first_abx_delay_hours,
  first_abx_time,

  (sofa_total_tvcov IS NULL) AS sofa_total_missing_orig,
  (meanbp_min_tvcov IS NULL) AS meanbp_missing_orig,
  (lactate_tvcov IS NULL) AS lactate_missing_orig,
  sofa_tvcov_missing,
  shock_tvcov_missing,
  lactate_tvcov_missing
FROM grp
ORDER BY stay_id, t_hour;

CREATE INDEX ix_ccw_model_input_stay_hour ON abx_tte_formal_v1.cov_tvcov_ccw_model_input (stay_id, t_hour);
CREATE INDEX ix_ccw_model_input_risk ON abx_tte_formal_v1.cov_tvcov_ccw_model_input (still_observed);

/* 5B audit: expected formal_v1 values after cleanup:
   rows 618375; stays 24735; risk rows 614307; sofa missing in risk 315; meanbp missing in risk about 10079.
*/
SELECT
  COUNT(*) AS n_rows,
  COUNT(DISTINCT stay_id) AS n_stays,
  COUNT(*) FILTER (WHERE still_observed = 1) AS risk_rows,
  COUNT(*) FILTER (WHERE still_observed = 1 AND sofa_total_tvcov IS NULL) AS sofa_missing_risk_after,
  COUNT(*) FILTER (WHERE still_observed = 1 AND meanbp_min_tvcov IS NULL) AS meanbp_missing_risk_after,
  COUNT(*) FILTER (WHERE still_observed = 1 AND lactate_tvcov IS NULL) AS lactate_missing_risk
FROM abx_tte_formal_v1.cov_tvcov_ccw_model_input;



/* FORMAL_V1 HARD CHECKS after STEP 5B.
   These values confirm grid completeness and agreed risk-set missingness behavior.
*/
DO $$
DECLARE
  bad_n int;
BEGIN
  SELECT CASE WHEN
      COUNT(*) = 618375
      AND COUNT(DISTINCT stay_id) = 24735
      AND COUNT(*) FILTER (WHERE still_observed = 1) = 614307
      AND COUNT(*) FILTER (WHERE still_observed = 1 AND sofa_total_tvcov IS NULL) = 315
      AND COUNT(*) FILTER (WHERE still_observed = 1 AND meanbp_min_tvcov IS NULL) = 10079
    THEN 0 ELSE 1 END
  INTO bad_n
  FROM abx_tte_formal_v1.cov_tvcov_ccw_model_input;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 hard check failed: CCW model input counts/missingness drifted from expected values.';
  END IF;
END $$;

/* =====================================================================================
   ── STEP 6 ── 结局表 (outcomes)
   ---------------------------------------------------------------------------------
   零点全部 = sepsis_t0

   (1) 30 天全因死亡 (主结局)
       - dod ∈ [t0_date, t0_date+30d]  -> event=1
       - 否则行政删失到 t0+30d (出院不构成删失,事件优先)
       - dod 按日级 (避免日级 vs 时分级混判)
       - fu_time 死亡者 = (dod_date - t0_date); 当天死=0.5 天兜底

   (2) 死亡归属 + ICU/住院死亡 (日级近似)
       in_hospital  = dod_date <= dischtime_date
       out_of_hosp  = dod_date >  dischtime_date
       disch_unknown = dischtime 为空 (单列一类,避免与 in/out 求和不闭合)

   (3) 器官恶化 (主口径 ΔSOFA >= 2)
       sofa_peak_72h = MAX(mimiciv_derived.sofa.sofa_24hours) in [t0, t0+72h]
       organ_deterioration_sofa = (sofa_peak_72h - cov_base.sofa_total_t0) >= 2
       敏感性口径 (新发升压/通气/RRT): 见 Methods,本表暂不实现

   (4) LOS (从 t0 起算, 天)
       原始抽取,主分析建模用 RMST 或 cause-specific (不用裸 LOS),
       已知 ~26 条 hosp_los 为负值 (t0 晚于 dischtime, 数据噪声), 建模时记 NULL。
   ===================================================================================== */

DROP TABLE IF EXISTS abx_tte_formal_v1.outcomes;
CREATE TABLE abx_tte_formal_v1.outcomes AS
WITH base AS (
  SELECT
    c.subject_id, c.hadm_id, c.stay_id,
    c.icu_intime, c.icu_outtime, c.sepsis_t0,
    c.abx_window, c.ccw_role,
    p.dod, a.dischtime,
    b.sofa_total_t0 AS sofa_base
  FROM abx_tte_formal_v1.cohort_exposure c
  LEFT JOIN mimiciv_hosp.patients   p ON p.subject_id = c.subject_id
  LEFT JOIN mimiciv_hosp.admissions a ON a.hadm_id    = c.hadm_id
  LEFT JOIN abx_tte_formal_v1.cov_base        b ON b.stay_id    = c.stay_id
),
sofa_peak AS (
  SELECT b.stay_id, MAX(s.sofa_24hours) AS sofa_peak_72h
  FROM base b
  LEFT JOIN mimiciv_derived.sofa s
         ON s.stay_id = b.stay_id
        AND s.endtime >= b.sepsis_t0
        AND s.endtime <= b.sepsis_t0 + INTERVAL '72 hours'
        AND s.sofa_24hours IS NOT NULL
  GROUP BY b.stay_id
)
SELECT
  b.subject_id, b.hadm_id, b.stay_id,
  b.sepsis_t0, b.icu_intime, b.icu_outtime,
  b.abx_window, b.ccw_role,
  b.dod, b.dischtime, b.sofa_base,

  /* (1) 30天死亡 */
  CASE WHEN b.dod IS NOT NULL
        AND b.dod::date >= b.sepsis_t0::date
        AND b.dod::date <= (b.sepsis_t0 + INTERVAL '30 days')::date
       THEN 1 ELSE 0 END AS death_30d,

  CASE WHEN b.dod IS NOT NULL
        AND b.dod::date >= b.sepsis_t0::date
        AND b.dod::date <= (b.sepsis_t0 + INTERVAL '30 days')::date
       THEN 'event' ELSE 'admin_censor_30d' END AS fu_status_30d,

  CASE WHEN b.dod IS NOT NULL
        AND b.dod::date >= b.sepsis_t0::date
        AND b.dod::date <= (b.sepsis_t0 + INTERVAL '30 days')::date
       THEN GREATEST((b.dod::date - b.sepsis_t0::date)::numeric, 0.5)
       ELSE 30.0 END AS fu_time_30d_days,

  /* (2) 死亡归属 + ICU/住院死亡 */
  CASE
    WHEN b.dod IS NULL THEN 'alive_or_no_record'
    WHEN b.dischtime IS NULL THEN 'disch_unknown'
    WHEN b.dod::date <= b.dischtime::date THEN 'in_hospital'
    ELSE 'out_of_hospital'
  END AS death_site,

  CASE WHEN b.dod IS NOT NULL AND b.icu_outtime IS NOT NULL
            AND b.dod::date <= b.icu_outtime::date
       THEN 1 ELSE 0 END AS death_icu,

  CASE WHEN b.dod IS NOT NULL AND b.dischtime IS NOT NULL
            AND b.dod::date <= b.dischtime::date
       THEN 1 ELSE 0 END AS death_hospital,

  /* (3) 器官恶化 (主口径) */
  sp.sofa_peak_72h,
  CASE
    WHEN b.sofa_base IS NULL OR sp.sofa_peak_72h IS NULL THEN NULL
    WHEN (sp.sofa_peak_72h - b.sofa_base) >= 2 THEN 1 ELSE 0
  END AS organ_deterioration_sofa,

  /* (4) LOS */
  EXTRACT(EPOCH FROM (b.icu_outtime - b.sepsis_t0)) / 86400.0 AS icu_los_from_t0_days,
  EXTRACT(EPOCH FROM (b.dischtime   - b.sepsis_t0)) / 86400.0 AS hosp_los_from_t0_days

FROM base b
LEFT JOIN sofa_peak sp ON sp.stay_id = b.stay_id;

CREATE INDEX ix_out_stay ON abx_tte_formal_v1.outcomes (stay_id);
CREATE INDEX ix_out_role ON abx_tte_formal_v1.outcomes (ccw_role);


/* =====================================================================================
   ── 最终验证 ── 跑完一次该看的所有核对数 (发老三/写 Methods 都用这套)
   ===================================================================================== */

-- 各表行数 (期望: cohort/cov_base/outcomes=24735; cov_tvcov/model_input=618375)
SELECT 'cohort_exposure' AS t, COUNT(*) FROM abx_tte_formal_v1.cohort_exposure
UNION ALL SELECT 'cov_base',  COUNT(*) FROM abx_tte_formal_v1.cov_base
UNION ALL SELECT 'cov_tvcov', COUNT(*) FROM abx_tte_formal_v1.cov_tvcov
UNION ALL SELECT 'cov_tvcov_ccw_model_input', COUNT(*) FROM abx_tte_formal_v1.cov_tvcov_ccw_model_input
UNION ALL SELECT 'outcomes',  COUNT(*) FROM abx_tte_formal_v1.outcomes;

-- 30天死亡 (期望 ≈ 19.7%)
SELECT
  COUNT(*) AS n, SUM(death_30d) AS n_dead,
  ROUND(100.0*SUM(death_30d)/COUNT(*),2) AS pct_death_30d
FROM abx_tte_formal_v1.outcomes;

-- 30天死亡内的院内/院外 (期望 in_hospital ≈ 79%, out_of_hospital ≈ 20%)
SELECT death_site, COUNT(*) FROM abx_tte_formal_v1.outcomes
WHERE death_30d = 1 GROUP BY death_site;

-- ccw_role 三档 (期望 analyzable 18167 / censor_noinit 786 / excluded 5782)
SELECT ccw_role, COUNT(*) FROM abx_tte_formal_v1.cohort_exposure GROUP BY ccw_role;


/* FINAL FORMAL_V1 HARD CHECKS.
   SQL data layer passing these checks does not validate the old OR/HR results;
   R-based CCW/IPCW/outcome models must be rerun from formal_v1 exports.
*/
DO $$
DECLARE
  bad_n int;
BEGIN
  WITH counts AS (
    SELECT
      (SELECT COUNT(*) FROM abx_tte_formal_v1.cohort_exposure) AS n_cohort,
      (SELECT COUNT(*) FROM abx_tte_formal_v1.cov_base) AS n_cov_base,
      (SELECT COUNT(*) FROM abx_tte_formal_v1.cov_tvcov) AS n_cov_tvcov,
      (SELECT COUNT(*) FROM abx_tte_formal_v1.cov_tvcov_ccw_model_input) AS n_ccw_input,
      (SELECT COUNT(*) FROM abx_tte_formal_v1.outcomes) AS n_outcomes,
      (SELECT SUM(death_30d) FROM abx_tte_formal_v1.outcomes) AS n_death30
  )
  SELECT CASE WHEN
      n_cohort = 24735
      AND n_cov_base = 24735
      AND n_cov_tvcov = 618375
      AND n_ccw_input = 618375
      AND n_outcomes = 24735
      AND n_death30 = 4876
    THEN 0 ELSE 1 END
  INTO bad_n
  FROM counts;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 final hard check failed: table counts or 30-day deaths drifted.';
  END IF;
END $$;

/* =====================================================================================
   == 完  ==
   数据管线齐活, 进入下一步: CCW 建模 (建议导出到 R: survival/ipw/WeightIt)
   ===================================================================================== */
