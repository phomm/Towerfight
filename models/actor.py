# coding=utf-8

from __future__ import annotations
from random import randint


class Actor:

    def __init__(self, name: str, level: int, symbol: int, color: str):
        self._name = name
        self._level = level
        self._symbol = symbol
        self._color = color

    @property
    def name(self):
        return self._name

    @property
    def level(self):
        return self._level

    @property
    def symbol(self):
        return self._symbol

    @property
    def color(self):
        return self._color

