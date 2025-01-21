# coding=utf-8

from __future__ import annotations
from models.actor import Actor


class Avatar(Actor):

    def fight(self, defender):
        if defender is None:
            return False
        if self._level > defender.real_level:
            self._level += defender.real_level
            return True
        return False


