# coding=utf-8

from models.avatar import Avatar
from models.actor import Actor
from models.enemy import Enemy
from random import randint


def build_pattern(x: int, y: int):
    return (x + y > 4 + x % 2) & (x % 2 != 0)


def get_cell(x: int, y: int):
    return x // 6, y // 4


class World:
    def __init__(self, width: int, height: int):
        self._avatar = Avatar("hero", randint(4, 7), 0xE002, "blue")
        self._avatar_cell = None
        self._width = width
        self._height = height
        self._army = {}
        self._fill_army()
        self._towers = {}
        self._fill_towers()

    @property
    def avatar(self):
        return self._avatar

    def put_avatar(self, cell):
        if self.towers.keys().__contains__(cell):
            self._avatar_cell = cell
        if self.army.keys().__contains__(cell):
            if type(self.army[cell]) is Enemy:
                self.army[cell].reveal()

    def fight(self, cell):
        if self.army.keys().__contains__(cell):
            if self.avatar.fight(self.army[cell]):
                self.army.pop(cell, None)
            else:
                self._avatar_cell = None

    def is_avatar_coords(self, x, y):
        return (x, y) == self._avatar_cell

    @property
    def towers(self):
        return self._towers

    def _fill_towers(self):
        for x in range(self._width):
            for y in range(self._height):
                if build_pattern(x, y):
                    self._towers[(x, y)] = True

    @property
    def army(self):
        return self._army

    def _fill_army(self):
        for x in range(self._width):
            for y in range(self._height):
                if build_pattern(x, y):
                    if (x == 1) and (y == 5):
                        self._avatar_cell = (x, y)
                    elif (x == 11) and (y == 0):
                        self.army[(x, y)] = Actor("dragon", 999, 0xE005, "purple")
                    elif randint(0, 5) != 0:
                        if randint(0, 2) == 1:
                            self.army[(x, y)] = Enemy("darkknight", x * x // 7 * randint(1, 9) + x * x, 0xE003, "orange")
                        else:
                            self.army[(x, y)] = Enemy("knight", x * x // 7 * randint(1, 9), 0xE004, "orange")

    def game_finish(self):
        results = {
            (True, False): "victory",
            (False, True): "defeat",
            (True, True): None,
        }
        avatar_exists = self._avatar_cell is not None
        enemies_exist = len(self.army.keys()) != 0
        return results[(avatar_exists, enemies_exist)]