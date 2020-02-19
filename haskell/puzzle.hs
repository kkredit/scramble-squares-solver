-- puzzle.hs
-- Solves a scramble-squares puzzle using idiomatic Haskell
-- Copyright (c) 2020, Kevin Kredit
-- License MIT

module Main
  where

main :: IO ()
main = putStrLn $ "Solution: " ++ show findSolution

data End = Tail | Head deriving (Eq, Show)
data Insect = Ant | Beetle | Dragonfly | Mantis deriving (Eq, Show)
data Edge = Edge { insect :: Insect, end :: End } deriving (Eq, Show)
data Piece = Piece Edge Edge Edge Edge deriving (Eq, Show)
type Board = [Piece]
type SetOfPieces = [Piece]

findSolution :: Board
findSolution = solution [] [
    makePiece Dragonfly Tail Ant Head Beetle Tail Mantis Head,
    makePiece Dragonfly Tail Ant Tail Beetle Head Mantis Tail,
    makePiece Dragonfly Tail Mantis Head Beetle Tail Ant Head,
    makePiece Dragonfly Tail Ant Head Mantis Head Ant Tail,
    makePiece Dragonfly Tail Ant Head Beetle Head Mantis Head,
    makePiece Dragonfly Head Beetle Tail Mantis Head Ant Tail,
    makePiece Dragonfly Head Mantis Tail Beetle Head Ant Tail,
    makePiece Dragonfly Head Mantis Head Beetle Head Dragonfly Tail,
    makePiece Beetle Tail Mantis Tail Ant Head Beetle Head
  ]

makePiece :: Insect -> End -> Insect -> End -> Insect -> End -> Insect -> End -> Piece
makePiece i1 e1 i2 e2 i3 e3 i4 e4 = Piece (Edge i1 e1) (Edge i2 e2) (Edge i3 e3) (Edge i4 e4)

solution :: Board -> SetOfPieces -> Board
solution b s
  | length b == 9 = b
  | otherwise =
      s
