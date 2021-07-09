# coding=utf-8

from bearlibterminal import terminal as blt
from bearlibterminal import bltutils
from common import utils
from scenes import game
from scenes import leaders


def reset():
    blt.set("window: size=80x25, cellsize=auto, title='Tower fight';"
            "input: filter={keyboard, mouse+}")
    blt.color("white")
    cw = blt.state(blt.TK_CELL_WIDTH)
    ch = blt.state(blt.TK_CELL_HEIGHT)
    width = blt.state(blt.TK_WIDTH)
    height = blt.state(blt.TK_HEIGHT)
    blt.set(f"U+E000: resources/Background.jpg, resize={width * cw}x{height * ch}, resize-filter=bilinear")
    blt.set(f"U+E001: resources/logo.png, resize={70 * cw}x{8 * ch}, resize-filter=bilinear")


def close():
    quit


def main():

    blt.open()

    menu_entries = (
        ("СТАРТ", game.scene),
        ("ЛИДЕРЫ", leaders.scene),
        ("ВЫХОД", close)
    )
    
    width = blt.state(blt.TK_WIDTH)
    height = blt.state(blt.TK_HEIGHT)

    reset()
    menu_index = 0
    menu_count = len(menu_entries)

    while True:
        blt.clear()

        blt.put(0, 0, 0xE000)
        blt.put(5, 3, 0xE001)

        for (i, entry) in enumerate(menu_entries):
            bkcolor = "red" if i == menu_index else "black"
            item_ypos = ((height // 3) * 3) + 2 * i
            blt.puts(0, i, bltutils.colored(entry[0], bkcolor), width, item_ypos, bltutils.align_center)
        blt.refresh()

        key = blt.read()

        if key in (blt.TK_ESCAPE, blt.TK_CLOSE):
            break
        elif key in (blt.TK_UP, blt.TK_DOWN):
            menu_index = utils.circulate(menu_count, menu_index, key == blt.TK_DOWN)

        elif key == blt.TK_ENTER:
            menu_entries[menu_index][1]()
            reset()

    blt.close()


if __name__ == "__main__":
    main()
