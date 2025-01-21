# coding=utf-8

from bearlibterminal import terminal as blt


align_center = blt.TK_ALIGN_MIDDLE | blt.TK_ALIGN_CENTER
box_whole = 0x2580
box_upper_half = 0x2588


def __tagged(tag, value, text):
    return f"[{tag}={value}]{text}[/{tag}]"


def colored(text, color):
    return __tagged("color", color, text)


def bkcolored(text, color):
    return __tagged("bkcolor", color, text)


def fullcolored(text, color, bkcolor):
    return bkcolored(colored(text, color), bkcolor)




