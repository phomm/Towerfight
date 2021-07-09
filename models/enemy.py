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
        operation = 0
        if self._level < 40:
            operation = randint(0, 1)
        elif self._level % 30 == 0:
            operation = 3
        elif self._level % 20 == 0:
            operation = 2
        if self._level <= 20:
            if self._level % 3 == 0:
                operation = 3
            elif self._level % 2 == 0:
                operation = 2
            else:
                operation = randint(0, 1)
        if operation == 0:
            a1 = randint(0, self._level)
            a2 = self._level - a1
            self._math = f"{a1}+{a2}"
        elif operation == 1:
            a1 = randint(0, 10)
            a2 = self._level + a1
            self._math = f"{a2}-{a1}"
        else:
            a2 = self._level // operation
            self._math = f"{a2}*{operation}"

    def hide(self):
        self._display_level = self._math

    def reveal(self):
        self._display_level = self._level

    @property
    def level(self):
        return self._display_level
