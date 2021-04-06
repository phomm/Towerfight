# coding=utf-8

from __future__ import division
from bearlibterminal import terminal as blt
from bearlibterminal import bltutils
from collections import namedtuple
from random import randint
from models.world import World, get_cell
from scenes import final


def scene():

    field_w = 12
    field_h = 6
    blt.set(f"window: size={field_w}x{field_h}, cellsize=128x128")
    cw = blt.state(blt.TK_CELL_WIDTH)
    ch = blt.state(blt.TK_CELL_HEIGHT)
    width = blt.state(blt.TK_WIDTH)
    height = blt.state(blt.TK_HEIGHT)
    blt.set(f"U+E000: resources/Background.jpg, resize={width * cw}x{height * ch}, resize-filter=bilinear")
    blt.set("U+E001: resources/Cell.png")
    blt.set("U+E002: resources/good.bmp")
    blt.set("U+E003: resources/bad.bmp")
    blt.set("U+E004: resources/neutral.bmp")
    blt.set("U+E005: resources/dragon.png")

    world = World(field_w, field_h)
    mlx = mly = 0

    result = None

    while True:
        blt.clear()

        blt.color("white")
        blt.put(0, 0, 0xE000)  # Background

        mx = blt.state(blt.TK_MOUSE_X)
        my = blt.state(blt.TK_MOUSE_Y)
        cell = get_cell(mx, my)
        blt.puts(1, 1, bltutils.colored("%s-%s-%s-%s-%s-%s" % (mx, my, mlx, mly, cell[0], cell[1]), "red"))

        for x in range(field_w):
            for y in range(field_h):
                if world.towers.keys().__contains__((x, y)):
                    blt.put(x * 6, y * 4, 0xE001)
                if world.army.keys().__contains__((x, y)):
                    blt.put(x * 6 + 2, y * 4 + 1, world.army[(x, y)].symbol)
                    blt.puts(x * 6 + 6, y * 4 + 1, bltutils.colored(world.army[(x, y)].level, world.army[(x, y)].color))

        blt.refresh()

        key = blt.read()

        if key in (blt.TK_CLOSE, blt.TK_ESCAPE):
            break
        elif key == blt.TK_MOUSE_LEFT:
            mlx = mx
            mly = my
            world.put_avatar(cell)
            result = world.game_finish()
            if result is not None:
                break

    if result is not None:
        final.scene(result, world.avatar.level)
