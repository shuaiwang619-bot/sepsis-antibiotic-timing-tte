from __future__ import annotations

from pathlib import Path
import math
import textwrap
from xml.sax.saxutils import escape

from PIL import Image, ImageDraw, ImageFont


FIGURE_DIR = Path(__file__).resolve().parent
OUT_PNG = FIGURE_DIR / "Supplementary_Figure_3_causal_dag_compact_draft_v4_icons_muted.png"
OUT_SVG = FIGURE_DIR / "Supplementary_Figure_3_causal_dag_compact_draft_v4_icons_muted.svg"

W, H = 4800, 2900


COLORS = {
    "ink": "#202A36",
    "muted": "#53606E",
    "grid": "#D9E0E7",
    "measured_fill": "#F2F6FA",
    "measured_stroke": "#5B739A",
    "residual_fill": "#FBF7EF",
    "residual_stroke": "#A5752A",
    "exposure_fill": "#F4F2FA",
    "exposure_stroke": "#5E4E86",
    "process_fill": "#F1F7F3",
    "process_stroke": "#4D765D",
    "outcome_fill": "#FBF6ED",
    "outcome_stroke": "#A56F28",
    "note_fill": "#F7FAFC",
    "note_stroke": "#6B7A8C",
}


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont:
    win = Path("C:/Windows/Fonts")
    candidates = [
        win / ("arialbd.ttf" if bold else "arial.ttf"),
        win / ("calibrib.ttf" if bold else "calibri.ttf"),
    ]
    for item in candidates:
        if item.exists():
            return ImageFont.truetype(str(item), size)
    return ImageFont.load_default()


F_TITLE = font(82, True)
F_SUB = font(40)
F_LABEL = font(48, True)
F_BOX = font(72, True)
F_NOTE = font(54)
F_FOOT = font(42)


def wrap_lines(value: str, chars: int) -> list[str]:
    out: list[str] = []
    for part in value.split("\n"):
        out.extend(textwrap.wrap(part, width=chars) or [""])
    return out


def text_size(draw: ImageDraw.ImageDraw, value: str, fnt: ImageFont.FreeTypeFont) -> tuple[int, int]:
    box = draw.textbbox((0, 0), value, font=fnt)
    return box[2] - box[0], box[3] - box[1]


def draw_centered_text(
    draw: ImageDraw.ImageDraw,
    rect: tuple[int, int, int, int],
    value: str,
    fnt: ImageFont.FreeTypeFont,
    fill: str,
    wrap: int,
    gap: int = 12,
) -> None:
    lines = wrap_lines(value, wrap)
    sizes = [text_size(draw, line, fnt) for line in lines]
    total_h = sum(h for _, h in sizes) + gap * (len(lines) - 1)
    x1, y1, x2, y2 = rect
    y = y1 + (y2 - y1 - total_h) / 2
    for line, (tw, th) in zip(lines, sizes):
        draw.text((x1 + (x2 - x1 - tw) / 2, y), line, font=fnt, fill=fill)
        y += th + gap


def draw_text_block(
    draw: ImageDraw.ImageDraw,
    rect: tuple[int, int, int, int],
    value: str,
    fnt: ImageFont.FreeTypeFont,
    fill: str,
    wrap: int,
    align: str = "left",
    gap: int = 12,
) -> None:
    lines = wrap_lines(value, wrap)
    sizes = [text_size(draw, line, fnt) for line in lines]
    total_h = sum(h for _, h in sizes) + gap * (len(lines) - 1)
    x1, y1, x2, y2 = rect
    y = y1 + (y2 - y1 - total_h) / 2
    for line, (tw, th) in zip(lines, sizes):
        if align == "center":
            x = x1 + (x2 - x1 - tw) / 2
        else:
            x = x1
        draw.text((x, y), line, font=fnt, fill=fill)
        y += th + gap


def draw_icon(draw: ImageDraw.ImageDraw, kind: str, cx: int, cy: int, color: str) -> None:
    r = 48
    w = 8
    if kind == "measured":
        # Sliders / measured covariates.
        for j, off in enumerate([-32, 0, 32]):
            y = cy + off
            draw.line((cx - r, y, cx + r, y), fill=color, width=w)
            knob_x = cx - 22 if j == 0 else cx + 18 if j == 1 else cx - 4
            draw.ellipse((knob_x - 15, y - 15, knob_x + 15, y + 15), outline=color, width=w)
    elif kind == "residual":
        # Warning triangle for residual confounding.
        pts = [(cx, cy - 58), (cx + 62, cy + 54), (cx - 62, cy + 54)]
        draw.line((pts[0], pts[1], pts[2], pts[0]), fill=color, width=w, joint="curve")
        draw.line((cx, cy - 20, cx, cy + 18), fill=color, width=w)
        draw.ellipse((cx - 5, cy + 35, cx + 5, cy + 45), fill=color)
    elif kind == "timing":
        # Clock for antibiotic timing window.
        draw.ellipse((cx - r, cy - r, cx + r, cy + r), outline=color, width=w)
        draw.line((cx, cy, cx, cy - 30), fill=color, width=w)
        draw.line((cx, cy, cx + 30, cy + 18), fill=color, width=w)
    elif kind == "pathway":
        # Linked nodes for clinical pathway.
        pts = [(cx - 46, cy + 30), (cx, cy - 22), (cx + 48, cy + 24)]
        draw.line((pts[0], pts[1], pts[2]), fill=color, width=w)
        for x, y in pts:
            draw.ellipse((x - 18, y - 18, x + 18, y + 18), fill="white", outline=color, width=w)
    elif kind == "outcome":
        # Bar chart for mortality outcome.
        draw.line((cx - 58, cy + 54, cx + 58, cy + 54), fill=color, width=w)
        bars = [(-40, 24, 22), (-6, -4, 50), (30, -34, 80)]
        for x, top, height in bars:
            draw.rectangle((cx + x, cy + 54 - height, cx + x + 18, cy + 54), fill=color)
    elif kind == "note":
        # Shield for design boundary.
        pts = [(cx, cy - 58), (cx + 50, cy - 34), (cx + 42, cy + 30), (cx, cy + 58), (cx - 42, cy + 30), (cx - 50, cy - 34)]
        draw.line((*pts[0], *pts[1], *pts[2], *pts[3], *pts[4], *pts[5], *pts[0]), fill=color, width=w, joint="curve")
        draw.line((cx - 24, cy, cx - 4, cy + 22, cx + 30, cy - 26), fill=color, width=w)


def dashed_round_rect(
    draw: ImageDraw.ImageDraw,
    rect: tuple[int, int, int, int],
    radius: int,
    fill: str,
    outline: str,
    width: int,
) -> None:
    draw.rounded_rectangle(rect, radius=radius, fill=fill, outline=outline, width=width)
    x1, y1, x2, y2 = rect
    dash, gap = 68, 42
    for x in range(x1 + 20, x2 - 20, dash + gap):
        draw.line((x, y1, min(x + gap, x2), y1), fill=fill, width=width + 6)
        draw.line((x, y2, min(x + gap, x2), y2), fill=fill, width=width + 6)
    for y in range(y1 + 20, y2 - 20, dash + gap):
        draw.line((x1, y, x1, min(y + gap, y2)), fill=fill, width=width + 6)
        draw.line((x2, y, x2, min(y + gap, y2)), fill=fill, width=width + 6)


def box(
    draw: ImageDraw.ImageDraw,
    rect: tuple[int, int, int, int],
    value: str,
    fill: str,
    stroke: str,
    wrap: int,
    dashed: bool = False,
    icon: str | None = None,
) -> None:
    if dashed:
        dashed_round_rect(draw, rect, 28, fill, stroke, 6)
    else:
        draw.rounded_rectangle(rect, radius=28, fill=fill, outline=stroke, width=6)
    if icon:
        x1, y1, x2, y2 = rect
        icon_x = x1 + 150
        icon_y = (y1 + y2) // 2
        draw_icon(draw, icon, icon_x, icon_y, stroke)
        text_rect = (x1 + 280, y1 + 20, x2 - 70, y2 - 20)
        draw_text_block(draw, text_rect, value, F_BOX, "#111827", wrap)
    else:
        draw_centered_text(draw, rect, value, F_BOX, "#111827", wrap)


def arrow_head(draw: ImageDraw.ImageDraw, start: tuple[int, int], end: tuple[int, int], color: str, size: int = 34) -> None:
    x1, y1 = start
    x2, y2 = end
    angle = math.atan2(y2 - y1, x2 - x1)
    pts = [
        (x2, y2),
        (x2 + size * math.cos(angle + math.pi * 0.82), y2 + size * math.sin(angle + math.pi * 0.82)),
        (x2 + size * math.cos(angle - math.pi * 0.82), y2 + size * math.sin(angle - math.pi * 0.82)),
    ]
    draw.polygon(pts, fill=color)


def line_segment(
    draw: ImageDraw.ImageDraw,
    p1: tuple[int, int],
    p2: tuple[int, int],
    color: str,
    width: int,
    dashed: bool,
) -> None:
    if not dashed:
        draw.line((*p1, *p2), fill=color, width=width)
        return
    x1, y1 = p1
    x2, y2 = p2
    dist = math.hypot(x2 - x1, y2 - y1)
    if dist == 0:
        return
    dash_len, gap_len = 64, 40
    step = dash_len + gap_len
    n = int(dist // step) + 1
    for i in range(n):
        a = min((i * step) / dist, 1)
        b = min((i * step + dash_len) / dist, 1)
        if a >= 1:
            break
        draw.line(
            (
                x1 + (x2 - x1) * a,
                y1 + (y2 - y1) * a,
                x1 + (x2 - x1) * b,
                y1 + (y2 - y1) * b,
            ),
            fill=color,
            width=width,
        )


def poly_arrow(
    draw: ImageDraw.ImageDraw,
    points: list[tuple[int, int]],
    color: str,
    width: int = 8,
    dashed: bool = False,
) -> None:
    for a, b in zip(points, points[1:]):
        line_segment(draw, a, b, color, width, dashed)
    arrow_head(draw, points[-2], points[-1], color)


def make_png() -> None:
    img = Image.new("RGB", (W, H), "white")
    draw = ImageDraw.Draw(img)

    draw.text((150, 100), "Conceptual causal structure for antibiotic timing and 30-day mortality", font=F_TITLE, fill=COLORS["ink"])
    draw.text(
        (150, 180),
        "Simplified directed acyclic graph illustrating measured confounding control, residual confounding, and the proposed clinical pathway.",
        font=F_SUB,
        fill=COLORS["muted"],
    )
    draw.line((150, 230, W - 150, 230), fill="#94A3B8", width=4)

    measured = (220, 560, 1660, 1160)
    residual = (220, 1390, 1660, 1990)
    exposure = (1970, 560, 3290, 1160)
    process = (1970, 1390, 3290, 1990)
    outcome = (3620, 980, 4620, 1600)
    note = (1380, 2240, 3820, 2545)

    labels = [
        ((225, 505), "Measured confounding control"),
        ((225, 1335), "Residual confounding"),
        ((1980, 505), "Treatment strategy"),
        ((3630, 925), "Outcome"),
        ((1980, 1335), "Clinical pathway"),
    ]
    for xy, value in labels:
        draw.text(xy, value, font=F_LABEL, fill=COLORS["ink"])

    box(
        draw,
        measured,
        "Measured baseline and time-varying factors\n(age, sex, race, admission category, Charlson index, SOFA, shock, lagged severity)",
        COLORS["measured_fill"],
        COLORS["measured_stroke"],
        27,
        icon="measured",
    )
    box(
        draw,
        residual,
        "Partially measured or unmeasured clinical drivers\n(diagnostic certainty, lactate, source control, antimicrobial appropriateness, staffing)",
        COLORS["residual_fill"],
        COLORS["residual_stroke"],
        28,
        dashed=True,
        icon="residual",
    )
    box(
        draw,
        exposure,
        "Antibiotic initiation window after suspected-infection t0",
        COLORS["exposure_fill"],
        COLORS["exposure_stroke"],
        20,
        icon="timing",
    )
    box(
        draw,
        process,
        "Duration of uncontrolled infection and early treatment-process organization",
        COLORS["process_fill"],
        COLORS["process_stroke"],
        21,
        icon="pathway",
    )
    box(draw, outcome, "30-day mortality", COLORS["outcome_fill"], COLORS["outcome_stroke"], 14, icon="outcome")
    box(
        draw,
        note,
        "CCW/IPCW aligns strategies at t0 and addresses informative artificial censoring, but cannot remove unmeasured confounding.",
        COLORS["note_fill"],
        COLORS["note_stroke"],
        51,
        icon="note",
    )

    grey = COLORS["measured_stroke"]
    orange = COLORS["residual_stroke"]
    purple = COLORS["exposure_stroke"]
    green = COLORS["process_stroke"]

    # Measured confounding paths use an upper lane and enter the outcome from the top.
    poly_arrow(draw, [(1660, 860), (1970, 860)], grey, 9)
    poly_arrow(draw, [(1660, 700), (1840, 420), (4120, 420), (4120, 980)], grey, 7)

    # Residual confounding paths use dashed outer lanes and enter from separate ports.
    poly_arrow(draw, [(1660, 1530), (1810, 1190), (1970, 1020)], orange, 8, dashed=True)
    poly_arrow(draw, [(1660, 1800), (1880, 2110), (4120, 2110), (4120, 1600)], orange, 8, dashed=True)

    # Proposed clinical pathway has the clearest, central route.
    poly_arrow(draw, [(2630, 1160), (2630, 1390)], purple, 12)
    poly_arrow(draw, [(3290, 1690), (3620, 1290)], green, 12)

    # Compact legend.
    legend_y = 2640
    draw.line((150, legend_y - 75, W - 150, legend_y - 75), fill=COLORS["grid"], width=3)
    draw.line((170, legend_y, 330, legend_y), fill=grey, width=9)
    draw.text((365, legend_y - 30), "measured confounding paths", font=F_FOOT, fill=COLORS["muted"])
    line_segment(draw, (1120, legend_y), (1280, legend_y), orange, 9, True)
    draw.text((1315, legend_y - 30), "partially measured / unmeasured paths", font=F_FOOT, fill=COLORS["muted"])
    draw.line((2580, legend_y, 2740, legend_y), fill=purple, width=10)
    draw.line((2760, legend_y, 2920, legend_y), fill=green, width=10)
    draw.text((2955, legend_y - 30), "proposed clinical pathway", font=F_FOOT, fill=COLORS["muted"])

    foot = "Abbreviations: CCW, clone-censor-weight; IPCW, inverse probability of censoring weighting; SOFA, Sequential Organ Failure Assessment; t0, time zero."
    draw.text((150, H - 92), foot, font=F_FOOT, fill=COLORS["muted"])

    img.save(OUT_PNG, dpi=(300, 300))


def svg_text_lines(x: int, y: int, lines: list[str], size: int, weight: int, fill: str, anchor: str = "middle") -> str:
    parts = [f'<text x="{x}" y="{y}" font-size="{size}" font-weight="{weight}" fill="{fill}" text-anchor="{anchor}">']
    line_height = int(size * 1.18)
    for i, line in enumerate(lines):
        dy = 0 if i == 0 else line_height
        parts.append(f'<tspan x="{x}" dy="{dy}">{escape(line)}</tspan>')
    parts.append("</text>")
    return "".join(parts)


def svg_icon(kind: str, cx: int, cy: int, color: str) -> str:
    if kind == "measured":
        parts = []
        for j, off in enumerate([-32, 0, 32]):
            y = cy + off
            knob_x = cx - 22 if j == 0 else cx + 18 if j == 1 else cx - 4
            parts.append(f'<line x1="{cx-48}" y1="{y}" x2="{cx+48}" y2="{y}" stroke="{color}" stroke-width="8" stroke-linecap="round"/>')
            parts.append(f'<circle cx="{knob_x}" cy="{y}" r="15" fill="white" stroke="{color}" stroke-width="8"/>')
        return "".join(parts)
    if kind == "residual":
        return (
            f'<path d="M {cx} {cy-58} L {cx+62} {cy+54} L {cx-62} {cy+54} Z" fill="none" stroke="{color}" stroke-width="8" stroke-linejoin="round"/>'
            f'<line x1="{cx}" y1="{cy-20}" x2="{cx}" y2="{cy+18}" stroke="{color}" stroke-width="8" stroke-linecap="round"/>'
            f'<circle cx="{cx}" cy="{cy+40}" r="6" fill="{color}"/>'
        )
    if kind == "timing":
        return (
            f'<circle cx="{cx}" cy="{cy}" r="48" fill="white" stroke="{color}" stroke-width="8"/>'
            f'<line x1="{cx}" y1="{cy}" x2="{cx}" y2="{cy-30}" stroke="{color}" stroke-width="8" stroke-linecap="round"/>'
            f'<line x1="{cx}" y1="{cy}" x2="{cx+30}" y2="{cy+18}" stroke="{color}" stroke-width="8" stroke-linecap="round"/>'
        )
    if kind == "pathway":
        return (
            f'<polyline points="{cx-46},{cy+30} {cx},{cy-22} {cx+48},{cy+24}" fill="none" stroke="{color}" stroke-width="8" stroke-linecap="round" stroke-linejoin="round"/>'
            f'<circle cx="{cx-46}" cy="{cy+30}" r="18" fill="white" stroke="{color}" stroke-width="8"/>'
            f'<circle cx="{cx}" cy="{cy-22}" r="18" fill="white" stroke="{color}" stroke-width="8"/>'
            f'<circle cx="{cx+48}" cy="{cy+24}" r="18" fill="white" stroke="{color}" stroke-width="8"/>'
        )
    if kind == "outcome":
        return (
            f'<line x1="{cx-58}" y1="{cy+54}" x2="{cx+58}" y2="{cy+54}" stroke="{color}" stroke-width="8" stroke-linecap="round"/>'
            f'<rect x="{cx-40}" y="{cy+32}" width="18" height="22" fill="{color}"/>'
            f'<rect x="{cx-6}" y="{cy+4}" width="18" height="50" fill="{color}"/>'
            f'<rect x="{cx+30}" y="{cy-26}" width="18" height="80" fill="{color}"/>'
        )
    if kind == "note":
        return (
            f'<path d="M {cx} {cy-58} L {cx+50} {cy-34} L {cx+42} {cy+30} L {cx} {cy+58} L {cx-42} {cy+30} L {cx-50} {cy-34} Z" fill="none" stroke="{color}" stroke-width="8" stroke-linejoin="round"/>'
            f'<polyline points="{cx-24},{cy} {cx-4},{cy+22} {cx+30},{cy-26}" fill="none" stroke="{color}" stroke-width="8" stroke-linecap="round" stroke-linejoin="round"/>'
        )
    return ""


def svg_text_block(x: int, y: int, lines: list[str], size: int, weight: int, fill: str) -> str:
    parts = [f'<text x="{x}" y="{y}" font-size="{size}" font-weight="{weight}" fill="{fill}" text-anchor="start">']
    line_height = int(size * 1.18)
    for i, line in enumerate(lines):
        dy = 0 if i == 0 else line_height
        parts.append(f'<tspan x="{x}" dy="{dy}">{escape(line)}</tspan>')
    parts.append("</text>")
    return "".join(parts)


def svg_box(
    rect: tuple[int, int, int, int],
    value: str,
    fill: str,
    stroke: str,
    wrap: int,
    dashed: bool = False,
    icon: str | None = None,
) -> str:
    x1, y1, x2, y2 = rect
    lines = wrap_lines(value, wrap)
    size = 72
    line_h = int(size * 1.18)
    text_h = len(lines) * line_h
    dash = ' stroke-dasharray="58 38"' if dashed else ""
    base = (
        f'<rect x="{x1}" y="{y1}" width="{x2-x1}" height="{y2-y1}" rx="28" fill="{fill}" '
        f'stroke="{stroke}" stroke-width="6"{dash}/>'
    )
    if icon:
        icon_x = x1 + 150
        icon_y = (y1 + y2) // 2
        text_x = x1 + 280
        start_y = int((y1 + y2 - text_h) / 2 + size)
        return base + svg_icon(icon, icon_x, icon_y, stroke) + svg_text_block(text_x, start_y, lines, size, 700, "#111827")
    start_y = int((y1 + y2 - text_h) / 2 + size)
    return base + svg_text_lines((x1 + x2) // 2, start_y, lines, size, 700, "#111827")


def svg_poly(points: list[tuple[int, int]], color: str, width: int, dashed: bool = False, marker: str = "arrow") -> str:
    dash = ' stroke-dasharray="64 40"' if dashed else ""
    point_str = " ".join(f"{x},{y}" for x, y in points)
    return f'<polyline points="{point_str}" fill="none" stroke="{color}" stroke-width="{width}" stroke-linecap="round" stroke-linejoin="round"{dash} marker-end="url(#{marker})"/>'


def make_svg() -> None:
    c = COLORS
    measured = (220, 560, 1660, 1160)
    residual = (220, 1390, 1660, 1990)
    exposure = (1970, 560, 3290, 1160)
    process = (1970, 1390, 3290, 1990)
    outcome = (3620, 980, 4620, 1600)
    note = (1380, 2240, 3820, 2545)
    grey = c["measured_stroke"]
    orange = c["residual_stroke"]
    purple = c["exposure_stroke"]
    green = c["process_stroke"]

    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
        '<rect width="100%" height="100%" fill="white"/>',
        '<style>text{font-family:Arial,Helvetica,sans-serif;letter-spacing:0}</style>',
        "<defs>",
        f'<marker id="arrow" markerWidth="16" markerHeight="16" refX="13" refY="8" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L16,8 L0,16 Z" fill="{grey}"/></marker>',
        f'<marker id="arrow_orange" markerWidth="16" markerHeight="16" refX="13" refY="8" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L16,8 L0,16 Z" fill="{orange}"/></marker>',
        f'<marker id="arrow_purple" markerWidth="16" markerHeight="16" refX="13" refY="8" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L16,8 L0,16 Z" fill="{purple}"/></marker>',
        f'<marker id="arrow_green" markerWidth="16" markerHeight="16" refX="13" refY="8" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L16,8 L0,16 Z" fill="{green}"/></marker>',
        "</defs>",
        f'<text x="150" y="100" font-size="82" font-weight="800" fill="{c["ink"]}">Conceptual causal structure for antibiotic timing and 30-day mortality</text>',
        f'<text x="150" y="180" font-size="40" font-weight="400" fill="{c["muted"]}">Simplified directed acyclic graph illustrating measured confounding control, residual confounding, and the proposed clinical pathway.</text>',
        '<line x1="150" y1="230" x2="4650" y2="230" stroke="#94A3B8" stroke-width="4"/>',
        f'<text x="225" y="505" font-size="48" font-weight="700" fill="{c["ink"]}">Measured confounding control</text>',
        f'<text x="225" y="1335" font-size="48" font-weight="700" fill="{c["ink"]}">Residual confounding</text>',
        f'<text x="1980" y="505" font-size="48" font-weight="700" fill="{c["ink"]}">Treatment strategy</text>',
        f'<text x="1980" y="1335" font-size="48" font-weight="700" fill="{c["ink"]}">Clinical pathway</text>',
        f'<text x="3630" y="925" font-size="48" font-weight="700" fill="{c["ink"]}">Outcome</text>',
        svg_box(measured, "Measured baseline and time-varying factors\n(age, sex, race, admission category, Charlson index, SOFA, shock, lagged severity)", c["measured_fill"], c["measured_stroke"], 27, icon="measured"),
        svg_box(residual, "Partially measured or unmeasured clinical drivers\n(diagnostic certainty, lactate, source control, antimicrobial appropriateness, staffing)", c["residual_fill"], c["residual_stroke"], 28, True, icon="residual"),
        svg_box(exposure, "Antibiotic initiation window after suspected-infection t0", c["exposure_fill"], c["exposure_stroke"], 20, icon="timing"),
        svg_box(process, "Duration of uncontrolled infection and early treatment-process organization", c["process_fill"], c["process_stroke"], 21, icon="pathway"),
        svg_box(outcome, "30-day mortality", c["outcome_fill"], c["outcome_stroke"], 14, icon="outcome"),
        svg_box(note, "CCW/IPCW aligns strategies at t0 and addresses informative artificial censoring, but cannot remove unmeasured confounding.", c["note_fill"], c["note_stroke"], 51, icon="note"),
        svg_poly([(1660, 860), (1970, 860)], grey, 9, marker="arrow"),
        svg_poly([(1660, 700), (1840, 420), (4120, 420), (4120, 980)], grey, 7, marker="arrow"),
        svg_poly([(1660, 1530), (1810, 1190), (1970, 1020)], orange, 8, True, "arrow_orange"),
        svg_poly([(1660, 1800), (1880, 2110), (4120, 2110), (4120, 1600)], orange, 8, True, "arrow_orange"),
        svg_poly([(2630, 1160), (2630, 1390)], purple, 12, marker="arrow_purple"),
        svg_poly([(3290, 1690), (3620, 1290)], green, 12, marker="arrow_green"),
        f'<line x1="150" y1="2565" x2="4650" y2="2565" stroke="{c["grid"]}" stroke-width="3"/>',
        f'<line x1="170" y1="2640" x2="330" y2="2640" stroke="{grey}" stroke-width="9"/>',
        f'<text x="365" y="2610" font-size="42" fill="{c["muted"]}">measured confounding paths</text>',
        f'<line x1="1120" y1="2640" x2="1280" y2="2640" stroke="{orange}" stroke-width="9" stroke-dasharray="64 40"/>',
        f'<text x="1315" y="2610" font-size="42" fill="{c["muted"]}">partially measured / unmeasured paths</text>',
        f'<line x1="2580" y1="2640" x2="2740" y2="2640" stroke="{purple}" stroke-width="10"/>',
        f'<line x1="2760" y1="2640" x2="2920" y2="2640" stroke="{green}" stroke-width="10"/>',
        f'<text x="2955" y="2610" font-size="42" fill="{c["muted"]}">proposed clinical pathway</text>',
        f'<text x="150" y="{H-92}" font-size="42" fill="{c["muted"]}">Abbreviations: CCW, clone-censor-weight; IPCW, inverse probability of censoring weighting; SOFA, Sequential Organ Failure Assessment; t0, time zero.</text>',
        "</svg>",
    ]
    OUT_SVG.write_text("\n".join(svg), encoding="utf-8")


if __name__ == "__main__":
    make_png()
    make_svg()
    print(OUT_PNG)
    print(OUT_SVG)
