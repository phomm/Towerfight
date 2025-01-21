# coding=utf-8

from __future__ import division
from bearlibterminal import terminal as blt
from bearlibterminal import bltutils
from collections import namedtuple

__leaders = []
max_length = 5


def add_leader(name: str, score: int):
    global __leaders
    __leaders.append((name, score))
    __leaders = get_leaders()[:max_length]


def get_leaders():
    __leaders.sort(key=lambda t: -t[1])
    return __leaders


def scene():

    cw = blt.state(blt.TK_CELL_WIDTH)
    ch = blt.state(blt.TK_CELL_HEIGHT)
    width = blt.state(blt.TK_WIDTH)
    height = blt.state(blt.TK_HEIGHT)
    blt.set(f"U+E000: resources/Background.jpg, resize={width * cw}x{height * ch}, resize-filter=bilinear")
    blt.set(f"U+E001: resources/leaders.png, resize={70 * cw}x{8 * ch}, resize-filter=bilinear")

    add_leader("wolf", 5)
    add_leader("snake", 3)
    add_leader("wolf", 7)
    add_leader("raven", 9)

    while True:
        blt.clear()

        blt.color("white")
        blt.put(0, 0, 0xE000)  # Background
        blt.put(5, 3, 0xE001)

        for (i, line) in enumerate(__leaders):
            item_ypos = 25 + i
            blt.puts(0, i, bltutils.colored(f"{line[0]}: {line[1]}", "red"), width, item_ypos, bltutils.align_center)
        blt.refresh()

        key = blt.read()

        if key in (blt.TK_CLOSE, blt.TK_ESCAPE):
            break
