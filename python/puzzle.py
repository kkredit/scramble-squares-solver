#!/usr/bin/env python3

"""
puzzle.py
Solves a scramble-squares puzzle using idiomatic Python
Copyright (c) 2021, Kevin Kredit
License MIT
"""

from enum import Enum
from collections import namedtuple


###############################################################################
##                                                                     Types ##
Insect = Enum("Insect", "ANT BEETLE DRAGONFLY MANTIS")
Position = Enum("Position", "TOP RIGHT BOTTOM LEFT")

# Piece = Insect[]
PlacedPiece = namedtuple("Piece", "piece rotation")
# PieceSet = Piece[]
Board = namedtuple("Board", "placedPieces pieceSet")


###############################################################################
##                                                                      Main ##
def main():
    # board = ()
    print("Working on a solution!")


###############################################################################
##                                                                 Functions ##


###############################################################################
##                                                                     Entry ##
if __name__ == "__main__":
    main()
