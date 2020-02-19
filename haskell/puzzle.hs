-- puzzle.hs
-- Solves a scramble-squares puzzle using idiomatic Haskell
-- Copyright (c) 2020, Kevin Kredit
-- License MIT

module Main
  where

main :: IO ()
main = putStrLn $ "Solution: " ++ showBoard findSolution

data End = Tail | Head deriving (Eq, Show)
data Edge = Ant End | Beetle End | Dragonfly End | Mantis End deriving (Eq, Show)
data Piece = Piece { name :: String, edges :: [Edge] } deriving (Eq, Show)
type Rotation = Int
type Board = [(Piece, Rotation)]
type SetOfPieces = [Piece]

findSolution :: Board
findSolution = solution ([], [
    Piece{name = "1", edges = [Dragonfly Tail, Ant Head, Beetle Tail, Mantis Head]},
    Piece{name = "2", edges = [Dragonfly Tail, Ant Tail, Beetle Head, Mantis Tail]},
    Piece{name = "3", edges = [Dragonfly Tail, Mantis Head, Beetle Tail, Ant Head]},
    Piece{name = "4", edges = [Dragonfly Tail, Ant Head, Mantis Head, Ant Tail]},
    Piece{name = "5", edges = [Dragonfly Tail, Ant Head, Beetle Head, Mantis Head]},
    Piece{name = "6", edges = [Dragonfly Head, Beetle Tail, Mantis Head, Ant Tail]},
    Piece{name = "7", edges = [Dragonfly Head, Mantis Tail, Beetle Head, Ant Tail]},
    Piece{name = "8", edges = [Dragonfly Head, Mantis Head, Beetle Head, Dragonfly Tail]},
    Piece{name = "9", edges = [Beetle Tail, Mantis Tail, Ant Head, Beetle Head]}
  ])

showBoard :: Board -> String
showBoard b = foldl (\acc p -> acc ++ showPiece p) "" b
  where showPiece (p, r) = name p ++ "," ++ show r ++ " "

solution :: (Board, SetOfPieces) -> Board
solution (b, s)
  | length b == 9 = b
  | otherwise =
      head [ solution (nb, ns) | (nb, ns) <- nextBoards, boardIsLegal nb ]
  where nextBoards = [(b, s)]

-- Only check latest piece
boardIsLegal :: Board -> Bool
boardIsLegal b
  | pos < 2 = True
  | otherwise = (topRow || matchesAbove) && (leftCol || matchesLeft)
  where pos = length b
        topRow = pos < 4
        leftCol = pos `mod` 3 == 1
        matchesAbove = True
        matchesLeft = True
