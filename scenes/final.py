# coding=utf-8

from __future__ import division
from bearlibterminal import terminal as blt
from bearlibterminal import bltutils
from collections import namedtuple
from scenes import leaders
from common import utils


def scene(game_result: str, level: int):
    max_chars = 32
    text = ""
    cw = blt.state(blt.TK_CELL_WIDTH)
    ch = blt.state(blt.TK_CELL_HEIGHT)
    width = blt.state(blt.TK_WIDTH)
    height = blt.state(blt.TK_HEIGHT)
    blt.set(f"U+E000: resources/Background.jpg, resize={width * cw}x{height * ch}, resize-filter=bilinear")
    blt.set(f"U+E001: resources/victory.png, resize={70 * cw}x{8 * ch}, resize-filter=bilinear")
    blt.set(f"U+E002: resources/defeat.png, resize={70 * cw}x{8 * ch}, resize-filter=bilinear")

    result = 0

    while True:
        blt.clear()

        blt.color("white")
        blt.put(0, 0, 0xE000)  # Background
        if game_result == "victory":
            blt.put(5, 3, 0xE001)
        if game_result == "defeat":
            blt.put(5, 3, 0xE002)

        blt.puts(15, 15, f"Получен уровень: {level}")
        blt.puts(15, 16, "Введите своё имя:")
        blt.puts(15, 17, text + "_")

        blt.refresh()

        key = blt.read()

        if key in (blt.TK_CLOSE, blt.TK_ESCAPE):
            break
        else:
            result, text = blt.read_str(15, 17, text, max_chars)
            text = utils.multiline_trim(text, "_", False, True)
        if result > 0:
            leaders.add_leader(text, level)
            break

    if result > 0:
        leaders.scene()
