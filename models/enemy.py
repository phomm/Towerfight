# coding=utf-8

from __future__ import annotations
from models.actor import Actor
from random import randint


class Enemy(Actor):

    def __init__(self, name: str, level: int, symbol: int, color: str):
        super().__init__(name, level, symbol, color)
        self._math = None
        self._display_level = None
        self.calc()
        self.hide()

    def calc(self):
        a1 = randint(0, self._level)
        a2 = self._level - a1
        self._math = f"{a1}+{a2}"

    def hide(self):
        self._display_level = self._math

    def reveal(self):
        self._display_level = self._level

    @property
    def level(self):
        return self._display_level