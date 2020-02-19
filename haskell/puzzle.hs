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
data Piece = Piece String Edge Edge Edge Edge deriving (Eq, Show)
type Rotation = Int
type Board = [(Piece, Rotation)]
type SetOfPieces = [Piece]

findSolution :: Board
findSolution = solution ([], [
    makePiece 1 (Dragonfly Tail) (Ant Head) (Beetle Tail) (Mantis Head),
    makePiece 2 (Dragonfly Tail) (Ant Tail) (Beetle Head) (Mantis Tail),
    makePiece 3 (Dragonfly Tail) (Mantis Head) (Beetle Tail) (Ant Head),
    makePiece 4 (Dragonfly Tail) (Ant Head) (Mantis Head) (Ant Tail),
    makePiece 5 (Dragonfly Tail) (Ant Head) (Beetle Head) (Mantis Head),
    makePiece 6 (Dragonfly Head) (Beetle Tail) (Mantis Head) (Ant Tail),
    makePiece 7 (Dragonfly Head) (Mantis Tail) (Beetle Head) (Ant Tail),
    makePiece 8 (Dragonfly Head) (Mantis Head) (Beetle Head) (Dragonfly Tail),
    makePiece 9 (Beetle Tail) (Mantis Tail) (Ant Head) (Beetle Head)
  ])

makePiece :: Int -> Edge -> Edge -> Edge -> Edge -> Piece
makePiece n = Piece (show n)

showBoard :: Board -> String
showBoard b = foldl (\acc p -> acc ++ showPiece p) "" b
  where showPiece ((Piece name _ _ _ _), r) = name ++ "," ++ show r ++ " "

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
