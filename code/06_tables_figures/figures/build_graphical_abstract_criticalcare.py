from pathlib import Path
import textwrap
from xml.sax.saxutils import escape

from PIL import Image, ImageDraw, ImageFont


OUT_DIR = Path(__file__).resolve().parent
PNG = OUT_DIR / "Graphical_Abstract_CriticalCare_920x300.png"
SVG = OUT_DIR / "Graphical_Abstract_CriticalCare_920x300.svg"

W, H = 920, 300

COL = {
    "ink": "#202A36",
    "muted": "#5A6877",
    "rule": "#D7DEE8",
    "blue": "#3F5A94",
    "blue_soft": "#F2F5FA",
    "green": "#4D765D",
    "green_soft": "#F1F7F3",
    "gold": "#A5752A",
    "gold_soft": "#FBF7EF",
    "grey": "#6B7A8C",
    "grey_soft": "#F7FAFC",
}


def font(size, bold=False):
    base = Path("C:/Windows/Fonts")
    for name in (("arialbd.ttf", "calibrib.ttf") if bold else ("arial.ttf", "calibri.ttf")):
        p = base / name
        if p.exists():
            return ImageFont.truetype(str(p), size)
    return ImageFont.load_default()


F_LABEL = font(9)
F_TITLE = font(22, True)
F_SUB = font(11)
F_HEAD = font(12, True)
F_BODY = font(11)
F_BODY_B = font(12, True)
F_BIG = font(17, True)
F_FOOT = font(8)


def draw_wrapped(draw, xy, text, fnt, fill, width, line_gap=2):
    x, y = xy
    lines = []
    for part in text.split("\n"):
        lines.extend(textwrap.wrap(part, width=width) or [""])
    for line in lines:
        draw.text((x, y), line, font=fnt, fill=fill)
        y += fnt.size + line_gap
    return y


def panel(draw, rect, label, title, fill, stroke):
    x1, y1, x2, y2 = rect
    draw.rounded_rectangle(rect, radius=10, fill=fill, outline=stroke, width=2)
    draw.rounded_rectangle((x1, y1, x2, y1 + 27), radius=10, fill=stroke, outline=stroke)
    draw.rectangle((x1, y1 + 17, x2, y1 + 27), fill=stroke)
    draw.text((x1 + 8, y1 + 7), label, font=F_HEAD, fill="white")
    draw.text((x1 + 28, y1 + 7), title, font=F_HEAD, fill="white")


def arrow(draw, x1, y1, x2, y2):
    draw.line((x1, y1, x2, y2), fill=COL["grey"], width=2)
    draw.polygon(((x2, y2), (x2 - 7, y2 - 4), (x2 - 7, y2 + 4)), fill=COL["grey"])


def make_png():
    img = Image.new("RGB", (W, H), "white")
    draw = ImageDraw.Draw(img)
    draw.text((20, 12), "Graphical abstract", font=F_LABEL, fill=COL["muted"])
    draw.text((20, 29), "Earlier vs. later antibiotic initiation in ICU sepsis", font=F_TITLE, fill=COL["ink"])
    draw.text((20, 57), "MIMIC-IV target trial emulation with eICU directional exploration", font=F_SUB, fill=COL["muted"])
    draw.line((20, 74, 900, 74), fill=COL["rule"], width=1)

    p1 = (20, 91, 285, 218)
    p2 = (327, 91, 592, 218)
    p3 = (634, 91, 900, 218)
    panel(draw, p1, "1", "Clinical question", COL["blue_soft"], COL["blue"])
    panel(draw, p2, "2", "Target-trial emulation", COL["green_soft"], COL["green"])
    panel(draw, p3, "3", "Main finding", COL["gold_soft"], COL["gold"])

    draw_wrapped(draw, (38, 130), "Suspected-infection t0\n0-3 h vs 3-6 h vs 6-12 h\n30-day mortality", F_BODY_B, COL["ink"], 30)
    draw_wrapped(draw, (345, 126), "MIMIC-IV primary analysis\nN = 18,953\nCCW/IPCW strategy emulation", F_BODY_B, COL["ink"], 32)
    draw.text((650, 126), "Compared with 0-3 h", font=F_BODY_B, fill=COL["ink"])
    draw.text((650, 150), "3-6 h: OR 1.21", font=F_BIG, fill=COL["ink"])
    draw.text((650, 173), "6-12 h: OR 1.29", font=F_BIG, fill=COL["ink"])
    draw.text((650, 199), "6-12 vs 3-6 h: OR 1.06", font=F_BODY_B, fill=COL["gold"])

    arrow(draw, p1[2], 154, p2[0], 154)
    arrow(draw, p2[2], 154, p3[0], 154)

    draw.rounded_rectangle((110, 236, 810, 273), radius=10, fill=COL["grey_soft"], outline=COL["grey"], width=1)
    draw.text(
        (128, 246),
        "Compared with 0-3 h, later windows had higher mortality; eICU was directionally compatible but uncertain.",
        font=F_BODY_B,
        fill=COL["ink"],
    )
    draw.text(
        (20, 286),
        "OR point estimates; 95% CIs in manuscript. Association, not proof of causation.",
        font=F_FOOT,
        fill=COL["muted"],
    )
    img.save(PNG, dpi=(96, 96))


def svg_text(x, y, text, size, weight, fill):
    return f'<text x="{x}" y="{y}" font-size="{size}" font-weight="{weight}" fill="{fill}">{escape(text)}</text>'


def make_svg():
    items = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
        '<rect width="100%" height="100%" fill="white"/>',
        '<style>text{font-family:Arial,Helvetica,sans-serif;letter-spacing:0}</style>',
        svg_text(20, 22, "Graphical abstract", 9, 400, COL["muted"]),
        svg_text(20, 50, "Earlier vs. later antibiotic initiation in ICU sepsis", 22, 800, COL["ink"]),
        svg_text(20, 68, "MIMIC-IV target trial emulation with eICU directional exploration", 11, 400, COL["muted"]),
        f'<line x1="20" y1="74" x2="900" y2="74" stroke="{COL["rule"]}" stroke-width="1"/>',
    ]
    for x, label, title, fill, stroke in [
        (20, "1", "Clinical question", COL["blue_soft"], COL["blue"]),
        (327, "2", "Target-trial emulation", COL["green_soft"], COL["green"]),
        (634, "3", "Main finding", COL["gold_soft"], COL["gold"]),
    ]:
        items += [
            f'<rect x="{x}" y="91" width="265" height="127" rx="10" fill="{fill}" stroke="{stroke}" stroke-width="2"/>',
            f'<rect x="{x}" y="91" width="265" height="27" rx="10" fill="{stroke}"/>',
            f'<rect x="{x}" y="108" width="265" height="10" fill="{stroke}"/>',
            svg_text(x + 8, 109, label, 12, 800, "white"),
            svg_text(x + 28, 109, title, 12, 800, "white"),
        ]
    items += [
        svg_text(38, 138, "Suspected-infection t0", 12, 800, COL["ink"]),
        svg_text(38, 158, "0-3 h vs 3-6 h vs 6-12 h", 12, 800, COL["ink"]),
        svg_text(38, 178, "30-day mortality", 12, 800, COL["ink"]),
        svg_text(345, 138, "MIMIC-IV primary analysis", 12, 800, COL["ink"]),
        svg_text(345, 159, "N = 18,953", 12, 800, COL["ink"]),
        svg_text(345, 180, "CCW/IPCW strategy emulation", 12, 800, COL["ink"]),
        svg_text(650, 138, "Compared with 0-3 h", 12, 800, COL["ink"]),
        svg_text(650, 163, "3-6 h: OR 1.21", 17, 800, COL["ink"]),
        svg_text(650, 186, "6-12 h: OR 1.29", 17, 800, COL["ink"]),
        svg_text(650, 207, "6-12 vs 3-6 h: OR 1.06", 12, 800, COL["gold"]),
        f'<line x1="285" y1="154" x2="327" y2="154" stroke="{COL["grey"]}" stroke-width="2"/><polygon points="327,154 320,150 320,158" fill="{COL["grey"]}"/>',
        f'<line x1="592" y1="154" x2="634" y2="154" stroke="{COL["grey"]}" stroke-width="2"/><polygon points="634,154 627,150 627,158" fill="{COL["grey"]}"/>',
        f'<rect x="110" y="236" width="700" height="37" rx="10" fill="{COL["grey_soft"]}" stroke="{COL["grey"]}" stroke-width="1"/>',
        svg_text(128, 260, "Compared with 0-3 h, later windows had higher mortality; eICU was directionally compatible but uncertain.", 12, 800, COL["ink"]),
        svg_text(20, 294, "OR point estimates; 95% CIs in manuscript. Association, not proof of causation.", 8, 400, COL["muted"]),
        "</svg>",
    ]
    SVG.write_text("\n".join(items), encoding="utf-8")


if __name__ == "__main__":
    make_png()
    make_svg()
    print(PNG)
    print(SVG)
