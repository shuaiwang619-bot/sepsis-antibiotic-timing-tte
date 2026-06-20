const fs = require("fs");
const path = require("path");

const figureDir = __dirname;
const analysisDir = path.resolve(figureDir, "..");

const files = {
  byArm: path.join(analysisDir, "formal_v1_ccw_positivity_weight_distribution_by_arm_m5.csv"),
  overlap: path.join(analysisDir, "formal_v1_ccw_positivity_continuous_overlap_m5.csv"),
  flags: path.join(analysisDir, "formal_v1_ccw_positivity_flag_summary.csv"),
};

const outSvg = path.join(figureDir, "Supplementary_Figure_3_ipcw_positivity_draft_v3.svg");
const outPng = path.join(figureDir, "Supplementary_Figure_3_ipcw_positivity_draft_v3.png");

function parseCsv(text) {
  const lines = text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").trim().split("\n");
  const header = parseCsvLine(lines[0]);
  return lines.slice(1).filter(Boolean).map((line) => {
    const values = parseCsvLine(line);
    const row = {};
    header.forEach((key, i) => {
      row[key] = values[i] ?? "";
    });
    return row;
  });
}

function parseCsvLine(line) {
  const values = [];
  let cur = "";
  let inQuotes = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"' && line[i + 1] === '"') {
      cur += '"';
      i++;
    } else if (ch === '"') {
      inQuotes = !inQuotes;
    } else if (ch === "," && !inQuotes) {
      values.push(cur);
      cur = "";
    } else {
      cur += ch;
    }
  }
  values.push(cur);
  return values;
}

function num(value) {
  const out = Number(value);
  return Number.isFinite(out) ? out : NaN;
}

function mean(values) {
  const clean = values.filter((v) => Number.isFinite(v));
  return clean.reduce((a, b) => a + b, 0) / clean.length;
}

function min(values) {
  return Math.min(...values.filter((v) => Number.isFinite(v)));
}

function max(values) {
  return Math.max(...values.filter((v) => Number.isFinite(v)));
}

function pct(value, digits = 1) {
  return `${value.toFixed(digits)}%`;
}

function esc(value) {
  return String(value)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

const byArm = parseCsv(fs.readFileSync(files.byArm, "utf8"));
const overlap = parseCsv(fs.readFileSync(files.overlap, "utf8"));
const flags = parseCsv(fs.readFileSync(files.flags, "utf8"));

const arms = ["0-3h", "3-6h", "6-12h"];
const armLabels = {
  "0-3h": "0-3 h",
  "3-6h": "3-6 h",
  "6-12h": "6-12 h",
};

const finalRows = byArm.filter((r) => r.weight_type === "final_ipcw");
const armSummary = arms.map((arm) => {
  const rows = finalRows.filter((r) => r.arm === arm);
  const n = Math.round(max(rows.map((r) => num(r.n_clones))));
  const essMin = min(rows.map((r) => num(r.ess)));
  return {
    arm,
    label: armLabels[arm],
    n,
    essMin,
    essPct: (essMin / n) * 100,
    p01: mean(rows.map((r) => num(r.p01))),
    p05: mean(rows.map((r) => num(r.p05))),
    p25: mean(rows.map((r) => num(r.p25))),
    median: mean(rows.map((r) => num(r.median))),
    p75: mean(rows.map((r) => num(r.p75))),
    p95: max(rows.map((r) => num(r.p95))),
    p99: max(rows.map((r) => num(r.p99))),
    maxWeight: max(rows.map((r) => num(r.max))),
    gt2: max(rows.map((r) => num(r.pct_gt_2))) * 100,
    gt5: max(rows.map((r) => num(r.pct_gt_5))) * 100,
    gt10: max(rows.map((r) => num(r.pct_gt_10))) * 100,
  };
});

const overlapSummary = Array.from(
  new Map(overlap.map((r) => [r.variable_label, r.variable_label])).values(),
).map((label) => {
  const rows = overlap.filter((r) => r.variable_label === label);
  return {
    label,
    p01p99: max(rows.map((r) => num(r.outside_common_p01_p99_pct))) * 100,
    p05p95: max(rows.map((r) => num(r.outside_common_p05_p95_pct))) * 100,
  };
});

const flagLabels = {
  final_ipcw_max_gt_10: "Maximum final IPCW <=10",
  final_ipcw_p99_gt_5: "99th percentile final IPCW <=5",
  final_ipcw_pct_gt_5_gt_1pct: "Final IPCW >5 in <=1% of clones",
  continuous_common_p01_p99_width_nonpositive: "Continuous p01-p99 overlap width >0",
  main_categorical_zero_count_arm: "No zero-count categorical arm",
};
const flagRows = flags.map((r) => ({
  check: flagLabels[r.check] || r.check.replace(/_/g, " "),
  status: r.status,
}));

const W = 3200;
const H = 2350;
const ink = "#12324d";
const muted = "#536575";
const grid = "#d7e1e8";
const pale = "#eef5f8";
const pale2 = "#f7f9fb";
const accent = "#0f4c75";
const accent2 = "#4f8db3";
const accent3 = "#9fbfd1";
const warn = "#d7892d";
const ok = "#2c7a57";

let svg = [];
function add(s) {
  svg.push(s);
}
function text(x, y, value, size = 34, weight = 400, fill = ink, anchor = "start") {
  add(`<text x="${x}" y="${y}" font-size="${size}" font-weight="${weight}" fill="${fill}" text-anchor="${anchor}">${esc(value)}</text>`);
}
function rect(x, y, w, h, fill = "none", stroke = "none", rx = 0, sw = 1) {
  add(`<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${rx}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"/>`);
}
function line(x1, y1, x2, y2, stroke = ink, sw = 2, dash = "") {
  add(`<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${stroke}" stroke-width="${sw}"${dash ? ` stroke-dasharray="${dash}"` : ""}/>`);
}
function circle(cx, cy, r, fill = accent, stroke = "none", sw = 1) {
  add(`<circle cx="${cx}" cy="${cy}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"/>`);
}
function diamond(cx, cy, r, fill = "white", stroke = accent, sw = 3) {
  add(`<path d="M ${cx} ${cy - r} L ${cx + r} ${cy} L ${cx} ${cy + r} L ${cx - r} ${cy} Z" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"/>`);
}
function tickLabel(x, y, value) {
  text(x, y, value, 28, 400, muted, "middle");
}
function panelLabel(x, y, label, title) {
  text(x, y, label, 38, 800, ink);
  text(x + 58, y, title, 38, 700, ink);
}

add(`<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">`);
add(`<rect width="${W}" height="${H}" fill="white"/>`);
add(`<style>text{font-family:Arial,Helvetica,sans-serif;} .small{font-size:26px;}</style>`);

text(130, 90, "IPCW weight distribution and positivity diagnostics", 54, 800, ink);
text(130, 140, "MIMIC-IV clone-censor-weight analysis; final stabilized IPCW among strategy-adherent cloned observations.", 31, 400, muted);
line(130, 170, W - 130, 170, "#8ca0af", 4);

// Panel A
const a = { x: 140, y: 245, w: 2920, h: 760 };
panelLabel(a.x, a.y, "A", "Final IPCW weight distribution by strategy");
const ax0 = a.x + 410;
const ax1 = a.x + 2220;
const ay0 = a.y + 120;
const ay1 = a.y + 580;
const xMaxA = 6.2;
const xScaleA = (v) => ax0 + (Math.max(0, Math.min(xMaxA, v)) / xMaxA) * (ax1 - ax0);
rect(ax0, ay0 - 45, ax1 - ax0, ay1 - ay0 + 95, "#fbfcfd", "#dfe7ec", 0, 2);
for (let t = 0; t <= 6; t += 1) {
  const x = xScaleA(t);
  line(x, ay0 - 45, x, ay1 + 50, t === 1 ? "#7c93a5" : grid, t === 1 ? 3 : 2, t === 1 ? "10 9" : "");
  tickLabel(x, ay1 + 92, String(t));
}
text(ax0, ay1 + 145, "Final IPCW weight", 30, 600, muted);
text(a.x + 10, ay0 - 70, "Strategy", 30, 700, ink);
text(ax1 + 185, ay0 - 70, "n", 30, 700, ink, "end");
text(ax1 + 540, ay0 - 70, "Minimum ESS", 30, 700, ink, "end");

armSummary.forEach((d, i) => {
  const y = ay0 + i * 170;
  rect(ax0, y - 50, ax1 - ax0, 100, i % 2 === 0 ? pale2 : "white", "none");
  text(a.x + 10, y + 11, d.label, 34, 700, ink);
  text(ax1 + 185, y + 11, d.n.toLocaleString("en-US"), 30, 500, ink, "end");
  text(ax1 + 540, y + 11, `${Math.round(d.essMin).toLocaleString("en-US")} (${pct(d.essPct)})`, 30, 500, ink, "end");
  line(xScaleA(d.p05), y, xScaleA(d.p95), y, "#263945", 4);
  line(xScaleA(d.p05), y - 18, xScaleA(d.p05), y + 18, "#263945", 4);
  line(xScaleA(d.p95), y - 18, xScaleA(d.p95), y + 18, "#263945", 4);
  rect(xScaleA(d.p25), y - 22, xScaleA(d.p75) - xScaleA(d.p25), 44, "#cfe1eb", accent, 5, 2);
  circle(xScaleA(d.median), y, 12, accent, "white", 3);
  diamond(xScaleA(d.p99), y, 14, "white", warn, 3);
  add(`<path d="M ${xScaleA(d.maxWeight) - 13} ${y - 13} L ${xScaleA(d.maxWeight) + 13} ${y + 13} M ${xScaleA(d.maxWeight) + 13} ${y - 13} L ${xScaleA(d.maxWeight) - 13} ${y + 13}" stroke="${warn}" stroke-width="4"/>`);
});
text(ax0 + 5, ay0 - 83, "Dashed line marks weight = 1.0", 27, 400, muted);
const legY = a.y + 790;
line(a.x + 520, legY, a.x + 640, legY, "#263945", 4);
text(a.x + 660, legY + 10, "5th-95th percentile", 27, 400, muted);
rect(a.x + 1015, legY - 20, 90, 40, "#cfe1eb", accent, 5, 2);
text(a.x + 1125, legY + 10, "IQR", 27, 400, muted);
circle(a.x + 1305, legY, 12, accent, "white", 3);
text(a.x + 1330, legY + 10, "Median", 27, 400, muted);
diamond(a.x + 1540, legY, 14, "white", warn, 3);
text(a.x + 1570, legY + 10, "99th percentile", 27, 400, muted);
add(`<path d="M ${a.x + 1855} ${legY - 13} L ${a.x + 1881} ${legY + 13} M ${a.x + 1881} ${legY - 13} L ${a.x + 1855} ${legY + 13}" stroke="${warn}" stroke-width="4"/>`);
text(a.x + 1900, legY + 10, "Maximum", 27, 400, muted);

// Panel B
const b = { x: 140, y: 1085, w: 1300, h: 590 };
panelLabel(b.x, b.y, "B", "Effective sample size retained");
const bx0 = b.x + 240;
const bx1 = b.x + 1160;
const by0 = b.y + 120;
const by1 = b.y + 430;
rect(bx0, by0 - 45, bx1 - bx0, by1 - by0 + 80, "#fbfcfd", "#dfe7ec", 0, 2);
const xScaleB = (v) => bx0 + (v / 100) * (bx1 - bx0);
for (const t of [0, 25, 50, 75, 100]) {
  const x = xScaleB(t);
  line(x, by0 - 45, x, by1 + 35, grid, 2);
  tickLabel(x, by1 + 78, String(t));
}
text((bx0 + bx1) / 2, by1 + 126, "ESS as percentage of cloned observations", 28, 600, muted, "middle");
armSummary.forEach((d, i) => {
  const y = by0 + i * 120;
  text(b.x + 10, y + 11, d.label, 32, 700, ink);
  rect(bx0, y - 27, xScaleB(d.essPct) - bx0, 54, i === 0 ? "#9fc4d7" : i === 1 ? "#6e9fba" : accent, "none", 4);
  text(xScaleB(d.essPct) + 18, y + 10, pct(d.essPct), 30, 700, ink);
});
text(b.x + 10, b.y + 520, "Minimum ESS across 5 imputed datasets.", 26, 400, muted);

// Panel C
const c = { x: 1580, y: 1085, w: 1480, h: 590 };
panelLabel(c.x, c.y, "C", "Extreme-weight frequency");
const cx0 = c.x + 250;
const cx1 = c.x + 1320;
const cy0 = c.y + 120;
const cy1 = c.y + 430;
rect(cx0, cy0 - 45, cx1 - cx0, cy1 - cy0 + 80, "#fbfcfd", "#dfe7ec", 0, 2);
const xScaleC = (v) => cx0 + (v / 8) * (cx1 - cx0);
for (const t of [0, 2, 4, 6, 8]) {
  const x = xScaleC(t);
  line(x, cy0 - 45, x, cy1 + 35, grid, 2);
  tickLabel(x, cy1 + 78, String(t));
}
text((cx0 + cx1) / 2, cy1 + 126, "Maximum percentage within arm across imputations", 28, 600, muted, "middle");
armSummary.forEach((d, i) => {
  const baseY = cy0 + i * 120;
  text(c.x + 10, baseY + 11, d.label, 32, 700, ink);
  const rows = [
    { label: ">2", value: d.gt2, color: accent },
    { label: ">5", value: d.gt5, color: warn },
    { label: ">10", value: d.gt10, color: "#9a3b3b" },
  ];
  rows.forEach((r, j) => {
    const y = baseY - 32 + j * 28;
    rect(cx0, y - 9, Math.max(1, xScaleC(r.value) - cx0), 18, r.color, "none", 2);
    text(cx0 - 28, y + 9, r.label, 22, 600, muted, "end");
    text(Math.max(xScaleC(r.value) + 12, cx0 + 18), y + 9, pct(r.value, r.value < 1 ? 2 : 1), 22, 500, ink);
  });
});

// Panel D
const dPanel = { x: 140, y: 1800, w: 2920, h: 390 };
panelLabel(dPanel.x, dPanel.y, "D", "Support and positivity checks");
const dx0 = dPanel.x + 60;
const dy0 = dPanel.y + 80;
text(dx0, dy0, "Continuous covariate support", 31, 700, ink);
const barX = dx0 + 520;
const barW = 1020;
const barScale = (v) => barX + (v / 15) * barW;
for (const t of [0, 5, 10, 15]) {
  const x = barScale(t);
  line(x, dy0 + 25, x, dy0 + 245, grid, 2);
  tickLabel(x, dy0 + 285, `${t}%`);
}
overlapSummary.forEach((row, i) => {
  const y = dy0 + 65 + i * 70;
  text(dx0, y + 10, row.label, 27, 600, ink);
  rect(barX, y - 20, barScale(row.p05p95) - barX, 24, accent3, "none", 2);
  rect(barX, y + 10, barScale(row.p01p99) - barX, 24, accent, "none", 2);
  text(barScale(row.p05p95) + 12, y, pct(row.p05p95), 22, 500, muted);
  text(barScale(row.p01p99) + 12, y + 30, pct(row.p01p99), 22, 500, ink);
});
rect(barX + 760, dy0 - 20, 26, 18, accent3, "none", 2);
text(barX + 800, dy0 - 3, "Outside common p05-p95", 24, 400, muted);
rect(barX + 760, dy0 + 15, 26, 18, accent, "none", 2);
text(barX + 800, dy0 + 32, "Outside common p01-p99", 24, 400, muted);

const flagX = dPanel.x + 1810;
text(flagX, dy0, "Predefined flag checks", 31, 700, ink);
flagRows.forEach((row, i) => {
  const y = dy0 + 48 + i * 46;
  rect(flagX, y - 27, 94, 34, row.status === "pass" ? "#e2f0e9" : "#f7ded8", "none", 17);
  text(flagX + 47, y - 3, row.status.toUpperCase(), 21, 800, row.status === "pass" ? ok : "#9a3b3b", "middle");
  text(flagX + 120, y - 3, row.check, 24, 500, ink);
});

line(130, H - 120, W - 130, H - 120, "#d7e1e8", 2);
text(130, H - 75, "Abbreviations: CCW, clone-censor-weight; ESS, effective sample size; IPCW, inverse probability of censoring weighting.", 27, 400, muted);
text(130, H - 35, "Tail percentages and percentiles are summarized across the 5 imputed datasets; no final IPCW exceeded 10 and the maximum observed weight was 5.97.", 27, 400, muted);

add("</svg>");

const svgText = svg.join("\n");
fs.writeFileSync(outSvg, svgText, "utf8");
console.log(`Wrote ${outSvg}`);
console.log(`PNG preview target: ${outPng}`);
