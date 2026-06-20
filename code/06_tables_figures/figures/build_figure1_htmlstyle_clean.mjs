import fs from "node:fs/promises";
import path from "node:path";
import { execFile as execFileCb } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

const execFile = promisify(execFileCb);
const outDir = path.dirname(fileURLToPath(import.meta.url));
const htmlPath = path.join(outDir, "Figure_1_htmlstyle_matchref.html");
const pngPath = path.join(outDir, "Figure_1_htmlstyle_matchref_600dpi.png");
const pdfPath = path.join(outDir, "Figure_1_htmlstyle_matchref.pdf");

const icon = (name) => {
  const common = `class="icon-svg" width="70" height="70" viewBox="0 0 64 64" aria-hidden="true"`;
  const paths = {
    database: `<g fill="currentColor"><ellipse cx="32" cy="13" rx="17" ry="7"/><path d="M15 13v8c0 4 8 7 17 7s17-3 17-7v-8c0 4-8 7-17 7s-17-3-17-7z"/><path d="M15 27v8c0 4 8 7 17 7s17-3 17-7v-8c0 4-8 7-17 7s-17-3-17-7z"/><path d="M15 41v8c0 4 8 7 17 7s17-3 17-7v-8c0 4-8 7-17 7s-17-3-17-7z"/></g>`,
    group: `<g fill="currentColor"><circle cx="27" cy="22" r="8"/><circle cx="44" cy="26" r="7"/><circle cx="14" cy="27" r="7"/><path d="M8 52c2-12 34-12 38 0H8z"/><path d="M34 52c1-8 16-9 22 0H34z"/><path d="M2 52c3-8 15-8 20-4l-1 4H2z"/></g>`,
    shield: `<path fill="currentColor" d="M32 5l23 9v15c0 15-10 25-23 30C19 54 9 44 9 29V14l23-9zm-4 34L19 30l-5 5 14 14 24-27-5-5-19 22z"/>`,
    document: `<path fill="currentColor" d="M16 7h24l10 10v40H16V7zm22 3v12h10L38 10zM23 29h18v4H23v-4zm0 10h20v4H23v-4zm0 10h14v4H23v-4z"/>`,
    clock: `<path fill="currentColor" d="M32 6a26 26 0 1026 26A26 26 0 0032 6zm0 46a20 20 0 1120-20 20 20 0 01-20 20zm3-34h-6v17l14 8 3-5-11-6V18z"/>`,
    scale: `<path fill="currentColor" d="M29 8h6v8h18v5H35v31h13v5H16v-5h13V21H11v-5h18V8zm-13 16l-9 16h18L16 24zm32 0l-9 16h18L48 24z"/>`,
    bars: `<path fill="currentColor" d="M9 53h47v5H9v-5zm6-18h8v16h-8V35zm13-11h8v27h-8V24zm13-12h8v39h-8V12z"/>`,
    search: `<path fill="currentColor" d="M27 8a19 19 0 1012 34l13 13 5-5-13-13A19 19 0 0027 8zm0 7a12 12 0 11-12 12 12 12 0 0112-12z"/>`,
    linechart: `<path fill="currentColor" d="M10 52h45v5H5V10h5v42zm7-7l-4-5 12-13 10 9 14-20 5 4-19 27-10-10-8 8z"/>`,
    book: `<path fill="currentColor" d="M8 12c9-4 17-3 24 2 7-5 15-6 24-2v41c-9-4-17-3-24 2-7-5-15-6-24-2V12zm6 7v26c5-1 10 0 15 3V20c-5-3-10-4-15-1zm21 1v28c5-3 10-4 15-3V19c-5-3-10-2-15 1z"/>`,
  };
  return `<svg ${common}>${paths[name]}</svg>`;
};

const box = ({ title, body, cls, iconName }) => `
  <div class="box ${cls}">
    <div class="icon">${icon(iconName)}</div>
    <div class="copy">
      <div class="box-title">${title}</div>
      <div class="box-body">${body.replaceAll("\n", "<br>")}</div>
    </div>
  </div>`;

const mimic = [
  { title: "Data source", body: "MIMIC-IV v3.1\nICU stays 94,458; admissions 546,028; patients 364,627", cls: "blue", iconName: "database" },
  { title: "Adult first ICU stay", body: "N = 54,551", cls: "blue", iconName: "group" },
  { title: "Sepsis-3 / SOFA-t0 screening", body: "N = 26,425", cls: "blue", iconName: "shield" },
  { title: "Suspected-infection t0 main cohort", body: "N = 24,735; 30-day deaths = 4,876\nTime zero: microbiological culture / suspected-infection t0", cls: "blue", iconName: "document" },
  { title: "Cohort entering CCW and timing windows", body: "Entered cloning stage: N = 18,953\nExcluded pre-t0 antibiotic use: N = 5,782\n0-3 h: 3,439; 3-6 h: 3,241; 6-12 h: 5,017\n>12-72 h: 6,470; no IV antibiotic within 72 h: 786", cls: "purple", iconName: "clock" },
  { title: "Main analysis", body: "Clone-censor-weight (CCW) + IPCW\nStrategies: 0-3 h, 3-6 h, and 6-12 h initiation\nMICE m = 5; weighted pooled logistic model", cls: "purple", iconName: "scale" },
  { title: "Main results", body: "3-6 h vs 0-3 h:   OR 1.21 (1.06-1.38),   p=0.003\n6-12 h vs 0-3 h:  OR 1.29 (1.14-1.45),   p<0.001\n6-12 h vs 3-6 h:  OR 1.06 (0.96-1.17),   p=0.233", cls: "orange", iconName: "bars" },
];

const eicu = [
  { title: "Data source", body: "eICU Collaborative Research Database\nTime anchor: ICU admission offset 0", cls: "green", iconName: "database" },
  { title: "First observed ICU stay per patient", body: "N = 139,367", cls: "green", iconName: "group" },
  { title: "Adult first observed ICU stay", body: "N = 138,855", cls: "green", iconName: "shield" },
  { title: "First ICU stay with explicit sepsis label", body: "eICU sepsis wide cohort: N = 21,326\n30-day in-hospital deaths = 4,027; missing outcome = 235", cls: "green", iconName: "search" },
  { title: "Antibiotic timing distribution", body: "Pre-t0 antibiotic: 7,038; no antibiotic record: 9,671\n0-3 h: 2,074; 3-6 h: 696; 6-12 h: 491\n>12-72 h: 1,035; >72 h: 321", cls: "purple", iconName: "search" },
  { title: "Primary directional analysis", body: "Arm 2: 0-3 h vs 3-12 h; after excluding missing outcomes N = 3,225\n0-3 h: 2,046, deaths 404; 3-12 h: 1,179, deaths 247\nAdjusted logistic regression + hospital random-intercept GLMM;\nCCW/IPCW sensitivity", cls: "purple", iconName: "linechart" },
  { title: "Directional results", body: "Adjusted logistic regression: OR 1.12 (0.92-1.37), p=0.256\nHospital random-intercept GLMM: OR 1.11 (0.91-1.36), p=0.291\nCCW/IPCW: OR 1.09 (0.92-1.28), p=0.323", cls: "orange", iconName: "bars" },
];

const column = (title, cls, rows) => `
  <section class="column ${cls}">
    <div class="col-header">${title}</div>
    ${rows.map((r, i) => `${box(r)}${i < rows.length - 1 ? `<div class="arrow"></div>` : ""}`).join("")}
  </section>`;

const html = `<!doctype html>
<html>
<head>
<meta charset="utf-8">
<style>
  * { box-sizing: border-box; }
  html, body { margin: 0; width: 3000px; height: 2300px; background: #ffffff; font-family: Arial, Helvetica, sans-serif; color: #111111; }
  body { padding: 24px 18px 0; }
  .title { text-align: center; font-size: 60px; line-height: 1.08; font-weight: 800; margin: 0 0 34px; letter-spacing: 0; color: #111; }
  .grid { display: grid; grid-template-columns: 1365px 1365px; column-gap: 180px; justify-content: center; }
  .col-header { height: 90px; border-radius: 8px; color: white; display: flex; align-items: center; justify-content: center; font-size: 39px; font-weight: 800; margin-bottom: 34px; }
  .mimic .col-header { background: #2f5597; }
  .eicu .col-header { background: #3f7158; }
  .box { min-height: 184px; border: 2px solid #b8c5d2; border-radius: 9px; display: grid; grid-template-columns: 170px 1fr; align-items: center; padding: 20px 30px 20px 16px; background: #f8fbff; }
  .box.blue { background: #f8fbff; border-color: #b9c8d8; color: #5279ab; }
  .box.green { background: #fbfdfb; border-color: #b8d0c0; color: #5c8c72; }
  .box.purple { background: #fbfaff; border-color: #c7bfd9; color: #5c4a86; min-height: 236px; }
  .box.orange { background: #fffaf1; border-color: #e7c894; color: #b1752b; min-height: 200px; }
  .icon { display: flex; align-items: center; justify-content: center; color: currentColor; }
  .icon-svg { width: 112px; height: 112px; }
  .box-title { font-size: 39px; font-weight: 800; color: #111111; line-height: 1.08; margin-bottom: 16px; }
  .orange .box-title { color: #9f6520; }
  .box-body { font-size: 30px; line-height: 1.32; color: #111111; font-weight: 500; white-space: normal; }
  .arrow { height: 42px; position: relative; }
  .arrow::before { content: ""; position: absolute; left: 50%; top: 2px; width: 8px; height: 25px; background: #687286; transform: translateX(-50%); border-radius: 999px; }
  .arrow::after { content: ""; position: absolute; left: 50%; top: 24px; transform: translateX(-50%); border-left: 12px solid transparent; border-right: 12px solid transparent; border-top: 18px solid #687286; }
  .note { margin: 36px auto 0; width: 2910px; border: 2px solid #b8c5d2; background: #fbfdff; border-radius: 9px; min-height: 226px; display: grid; grid-template-columns: 170px 1fr; align-items: center; padding: 28px 32px 28px 16px; color: #5a78a5; }
  .note .icon-svg { width: 112px; height: 112px; }
  .note-title { font-size: 39px; font-weight: 800; margin-bottom: 16px; color: #111111; }
  .note-body { font-size: 30px; line-height: 1.32; color: #111111; font-weight: 500; white-space: normal; }
  .foot { margin-top: 24px; font-size: 25px; color: #333333; }
</style>
</head>
<body>
  <h1 class="title">Study flow: MIMIC-IV target trial emulation and eICU directional exploration</h1>
  <main class="grid">
    ${column("MIMIC-IV main target trial emulation", "mimic", mimic)}
    ${column("eICU directional exploration", "eicu", eicu)}
  </main>
  <section class="note">
    <div class="icon">${icon("book")}</div>
    <div>
      <div class="note-title">Interpretive boundary</div>
      <div class="note-body">MIMIC-IV was the main analysis because suspected-infection t0 could not be reliably reconstructed in eICU, eICU was used only for ICU-admission-anchored directional exploration, not same-definition replication.</div>
    </div>
  </section>
  <div class="foot">Abbreviations: CCW = clone-censor-weight; IPCW = inverse probability of censoring weighting; MICE = multiple imputation by chained equations.</div>
</body>
</html>`;

await fs.writeFile(htmlPath, html, "utf8");
const chrome = "C:/Program Files/Google/Chrome/Application/chrome.exe";
const fileUrl = `file:///${htmlPath.replaceAll("\\", "/").replaceAll(" ", "%20")}`;
await execFile(chrome, [
  "--headless=new",
  "--disable-gpu",
  "--hide-scrollbars",
  "--force-device-scale-factor=2",
  "--window-size=3000,2300",
  `--screenshot=${pngPath}`,
  fileUrl,
], { timeout: 120000 });
await execFile(chrome, [
  "--headless=new",
  "--disable-gpu",
  "--print-to-pdf-no-header",
  `--print-to-pdf=${pdfPath}`,
  fileUrl,
], { timeout: 120000 });

console.log("html", htmlPath);
console.log("png", pngPath);
console.log("pdf", pdfPath);
console.log("checks", 3439 + 3241 + 5017 + 6470 + 786, 7038 + 9671 + 2074 + 696 + 491 + 1035 + 321, 2046 + 1179);
