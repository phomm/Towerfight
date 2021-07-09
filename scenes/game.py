# coding=utf-8

from __future__ import division
from bearlibterminal import terminal as blt
from bearlibterminal import bltutils
from collections import namedtuple
from random import randint
from models.world import World, get_cell
from models.avatar import Avatar
from models.actor import Actor
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
    blt.set("U+E006: resources/sword.bmp")
    blt.set("U+E007: resources/shield.bmp")

    world = World(field_w, field_h)

    result = None

    fight_state = 0

    while True:
        blt.clear()

        blt.color("white")
        blt.put(0, 0, 0xE000)  # Background

        mx = blt.state(blt.TK_MOUSE_X)
        my = blt.state(blt.TK_MOUSE_Y)
        cell = get_cell(mx, my)
        # blt.puts(1, 1, bltutils.colored("%s-%s-%s-%s-%s-%s" % (mx, my, mlx, mly, cell[0], cell[1]), "red"))

        for x in range(field_w):
            for y in range(field_h):
                if world.towers.keys().__contains__((x, y)):
                    blt.put(x * 6, y * 4, 0xE001)
                if world.is_avatar_coords(x, y):
                    draw_actor(x, y, world.avatar)
                if world.army.keys().__contains__((x, y)):
                    draw_actor(x, y, world.army[(x, y)])

        blt.refresh()

        key = blt.read()

        if key in (blt.TK_CLOSE, blt.TK_ESCAPE):
            break
        elif key == blt.TK_MOUSE_LEFT and fight_state == 0:
            mlx = mx
            mly = my
            world.put_avatar(cell)
            fight_state = 1
            blt.delay(500)

        elif fight_state == 1:
            world.fight(cell)
            fight_state = 2
            blt.delay(500)

        elif fight_state == 2:
            result = world.game_finish()
            if result is not None:
                break
            else:
                fight_state = 0

    if result is not None:
        final.scene(result, world.avatar.level)


def draw_actor(x: int, y: int, actor):
    if actor is None:
        return
    x_shift = 1 if type(actor) is Avatar else 5
    blt.put(x * 6 + x_shift, y * 4 + 1, actor.symbol)
    blt.puts(x * 6 + x_shift, y * 4, bltutils.colored(actor.level, actor.color))