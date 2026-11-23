#!/usr/bin/env python3
# tools/benchmark/film.py --- Make a film of the replace run, the editors side by side.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Turn the recordings of a replace run into a film of the editors side by side.

Usage: python3 -I tools/benchmark/film.py RESULTS.jsonl CASTS-DIRECTORY FILM.mp4
           [--real 90] [--speed 30] [--fps 10] [--font FILE] [--size 16]

RESULTS.jsonl is what the harness wrote for the run (harness --scenario
replace), CASTS-DIRECTORY what it recorded (--record): one asciicast for each
editor. tools/benchmark/run.sh --replace --film makes both and the film.

Each editor's screen is played back from its recording in a pane of its own,
two by two, each from its own start, so that all start together: the film
shows what each would have shown in the same seconds. The first --real
seconds play at their true speed, the rest --speed times as fast, which the
clock at the top says. Above each pane are the editor's name and its own
clock; below it, the last keys sent, in red how long a key has gone
without the editor writing anything to the screen (from half a second on),
and, once its run has ended, how. The
film is encoded by ffmpeg (H.264 by OpenH264, or VP9 for a .webm name).

Pillow draws the frames; it is used here only, never by the harness.
"""

import argparse
import json
import os
import subprocess
import sys

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from screen import Screen  # noqa: E402

BACKGROUND = (24, 24, 27)
FOREGROUND = (212, 212, 216)
CAPTION = (161, 161, 170)
ENDED = (248, 113, 113)
DONE = (134, 239, 172)
BASE16 = [(0, 0, 0), (205, 49, 49), (13, 188, 121), (229, 229, 16), (36, 114, 200), (188, 63, 188), (17, 168, 205),
          (229, 229, 229), (102, 102, 102), (241, 76, 76), (35, 209, 139), (245, 245, 67), (59, 142, 234),
          (214, 112, 214), (41, 184, 219), (255, 255, 255)]
KEY_NAMES = {"\r": "RET", "\x1b": "Esc", "\x7f": "DEL", "\t": "TAB", " ": "SPC"}


def palette(index):
    """Return the colour of xterm's 256-colour palette at INDEX."""
    if index < 16:
        return BASE16[index]
    if index < 232:
        index -= 16
        steps = [0, 95, 135, 175, 215, 255]
        return steps[index // 36], steps[index // 6 % 6], steps[index % 6]
    grey = 8 + (index - 232) * 10
    return grey, grey, grey


def colour(value, default):
    """Return the colour VALUE names in a screen's pen (None, a palette number or #rrggbb), DEFAULT for None."""
    if value is None:
        return default
    if isinstance(value, int):
        return palette(value)
    return tuple(int(value[i:i + 2], 16) for i in (1, 3, 5))


def key_words(keys):
    """Return KEYS, what was sent to an editor, as a person reads them: control keys named, C-x for Control-x."""
    words = []
    for char in keys:
        if char in KEY_NAMES:
            words.append(KEY_NAMES[char])
        elif ord(char) < 32:
            words.append("C-" + chr(ord(char) + 96))
        else:
            words.append(char)
    return " ".join(words)


class Pane:
    """One editor's recording, played into a screen of its own up to a time asked for."""

    def __init__(self, path):
        """Read the asciicast at PATH."""
        with open(path) as f:
            header = json.loads(f.readline())
            self.events = [json.loads(line) for line in f if line.strip()]
        self.screen = Screen(header["height"], header["width"], colours=True)
        self.end = self.events[-1][0] if self.events else 0.0
        self.at = 0
        self.keys = ""
        self.key_at = None
        self.output_at = 0.0
        self.changed = True

    def waiting(self, seconds):
        """How long, at SECONDS, a key sent has gone without the editor writing anything since; 0 when it has."""
        if self.key_at is None or self.output_at >= self.key_at:
            return 0.0
        return seconds - self.key_at

    def play_to(self, seconds):
        """Play the recording up to SECONDS from its start; note whether the screen changed."""
        while self.at < len(self.events) and self.events[self.at][0] <= seconds:
            _, kind, data = self.events[self.at]
            if kind == "o":
                self.screen.feed(data.encode("utf-8"))
                self.output_at = self.events[self.at][0]
                self.changed = True
            elif kind == "i":
                self.keys = (self.keys + data)[-24:]
                if self.key_at is None or self.output_at >= self.key_at:
                    self.key_at = self.events[self.at][0]
                self.changed = True
            self.at += 1


class Film:
    """The frames of the film, drawn with FONT (a font file) at SIZE pixels."""

    def __init__(self, panes, names, outcomes, font, size, title):
        """Draw PANES (Pane objects) under NAMES, each ending as OUTCOMES says, under TITLE."""
        self.panes, self.names, self.outcomes, self.title = panes, names, outcomes, title
        self.font = ImageFont.truetype(font, size)
        bold = font.replace("Regular", "Bold")
        self.bold = ImageFont.truetype(bold, size) if os.path.exists(bold) else self.font
        self.small = ImageFont.truetype(font, size + 4)
        # the keys typed, read at a glance below each editor
        self.caption = ImageFont.truetype(bold if os.path.exists(bold) else font, size + 10)
        self.cell_w = max(1, round(self.font.getlength("M")))
        self.cell_h = round(size * 1.25)
        screen = panes[0].screen
        self.pane_w = screen.columns * self.cell_w
        self.pane_h = screen.rows * self.cell_h
        self.bar = round(size * 2.2)
        self.header = round(size * 3)
        self.width = 2 * self.pane_w
        self.height = self.header + 2 * (self.pane_h + 2 * self.bar)
        self.width += self.width % 2
        self.height += self.height % 2
        self.images = [None] * len(panes)

    def draw_screen(self, screen):
        """Return an image of SCREEN's cells, each row drawn in runs of one pen."""
        image = Image.new("RGB", (self.pane_w, self.pane_h), BACKGROUND)
        draw = ImageDraw.Draw(image)
        for r, cells in enumerate(screen.grid):
            y = r * self.cell_h
            c = 0
            while c < len(cells):
                pen = cells[c][1]
                start = c
                text = []
                while c < len(cells) and cells[c][1] == pen:
                    text.append(cells[c][0] or "")
                    c += 1
                fg = colour(pen[0], FOREGROUND)
                bg = colour(pen[1], BACKGROUND)
                if pen[3]:
                    fg, bg = bg, fg
                if bg != BACKGROUND:
                    draw.rectangle([start * self.cell_w, y, c * self.cell_w - 1, y + self.cell_h - 1], fill=bg)
                run = "".join(text)
                if run.strip():
                    draw.text((start * self.cell_w, y), run, font=self.bold if pen[2] else self.font, fill=fg)
        return image

    def frame(self, seconds, speed):
        """Return the frame at SECONDS from the start, the film then running at SPEED times true time."""
        image = Image.new("RGB", (self.width, self.height), BACKGROUND)
        draw = ImageDraw.Draw(image)
        clock = f"{int(seconds // 60)}:{seconds % 60:04.1f}"
        title = f"{self.title}    {clock}    {'true speed' if speed == 1 else f'{speed}x speed'}"
        draw.text((self.bar // 2, self.header // 4), title, font=self.small, fill=FOREGROUND)
        for i, pane in enumerate(self.panes):
            pane.play_to(seconds)
            if pane.changed or self.images[i] is None:
                self.images[i] = self.draw_screen(pane.screen)
                pane.changed = False
            x = (i % 2) * self.pane_w
            y = self.header + (i // 2) * (self.pane_h + 2 * self.bar)
            own = min(seconds, pane.end)
            draw.text((x + self.bar // 2, y + self.bar // 5), f"{self.names[i]}    {int(own // 60)}:{own % 60:04.1f}",
                      font=self.small, fill=FOREGROUND)
            image.paste(self.images[i], (x, y + self.bar))
            below = y + self.bar + self.pane_h + self.bar // 5
            if seconds > pane.end:
                ok = self.outcomes[i] == "done"
                draw.text((x + self.bar // 2, below), "done" if ok else f"ended: {self.outcomes[i]}",
                          font=self.small, fill=DONE if ok else ENDED)
            elif pane.keys:
                caption = "keys: " + key_words(pane.keys[-16:])
                draw.text((x + self.bar // 2, below - self.bar // 10), caption, font=self.caption, fill=CAPTION)
                waiting = pane.waiting(seconds)
                if waiting >= 0.5:
                    left = x + self.bar // 2 + draw.textlength(caption + "   ", font=self.caption)
                    draw.text((left, below - self.bar // 10), f"no answer for {waiting:.1f} s", font=self.caption,
                              fill=ENDED)
        return image


def main():
    """Read the arguments, play the recordings and encode the film."""
    parser = argparse.ArgumentParser(description="Make a film of the replace run, the editors side by side.")
    parser.add_argument("results")
    parser.add_argument("casts")
    parser.add_argument("film")
    parser.add_argument("--real", type=float, default=90)
    parser.add_argument("--speed", type=int, default=30)
    parser.add_argument("--fps", type=int, default=10)
    parser.add_argument("--font", default=subprocess.run(["fc-match", "-f", "%{file}", "monospace:style=Regular"],
                                                         capture_output=True, text=True).stdout)
    parser.add_argument("--size", type=int, default=16)
    args = parser.parse_args()
    meta, records = None, {}
    with open(args.results) as f:
        for line in f:
            item = json.loads(line)
            if "meta" in item:
                meta = item["meta"]
            else:
                records[item["editor"]] = item
    editors = [e for e in meta["editors"] if os.path.exists(os.path.join(args.casts, e + ".cast"))][:4]
    panes = [Pane(os.path.join(args.casts, e + ".cast")) for e in editors]
    names = [meta["editors"][e]["name"] for e in editors]
    outcomes = ["done" if records.get(e, {}).get("status") == "ok" else records.get(e, {}).get("status", "?")
                for e in editors]
    entry = meta["files"][0]
    title = (f"fox replaced by cat in {entry['path']} ({entry['size'] / 2 ** 30:.1f} GiB, "
             f"{entry['fox_count']:,} times), then a sentence typed in a new buffer, saved, undone")
    film = Film(panes, names, outcomes, args.font, args.size, title)
    end = max(p.end for p in panes) + 3
    codec = ["-c:v", "libvpx-vp9", "-b:v", "3M"] if args.film.endswith(".webm") else \
        ["-c:v", "libopenh264", "-b:v", "6M", "-pix_fmt", "yuv420p"]
    encoder = subprocess.Popen(["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24",
                                "-s", f"{film.width}x{film.height}", "-r", str(args.fps), "-i", "-"] + codec
                               + [args.film], stdin=subprocess.PIPE)
    seconds = 0.0
    while seconds <= end:
        speed = 1 if seconds < args.real else args.speed
        encoder.stdin.write(film.frame(seconds, speed).tobytes())
        seconds += speed / args.fps
    encoder.stdin.close()
    sys.exit(encoder.wait())


if __name__ == "__main__":
    main()
