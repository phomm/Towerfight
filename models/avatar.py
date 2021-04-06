# coding=utf-8

from __future__ import annotations
from models.actor import Actor


class Avatar(Actor):

    def fight(self, defender):
        if defender is None:
            return False
        if self.level > defender.level:
            self._level += defender.level
            return True
        return False


