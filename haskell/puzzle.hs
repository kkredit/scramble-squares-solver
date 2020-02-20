-- puzzle.hs
-- Solves a scramble-squares puzzle using idiomatic Haskell
-- Copyright (c) 2020, Kevin Kredit
-- License MIT

module Main
  where

import Data.Maybe

main :: IO ()
main = putStrLn $ "Solution: " ++ showBoard findSolution

data End = Tail | Head deriving (Eq, Show)
data Insect = Ant | Beetle | Dragonfly | Mantis deriving (Eq, Show)
data Edge = Edge { insect :: Insect, end :: End } deriving (Eq, Show)
data Piece = Piece {
    name :: String
  , rotation :: Int
  , top :: Edge
  , right :: Edge
  , bottom :: Edge
  , left:: Edge
  } deriving (Eq, Show)
type Board = [Piece]
type SetOfPieces = [Piece]
type State = (Board, SetOfPieces)

findSolution :: Board
findSolution = solution ([], [
    makePiece 1 Dragonfly Tail Ant Head Beetle Tail Mantis Head
  , makePiece 2 Dragonfly Tail Ant Tail Beetle Head Mantis Tail
  , makePiece 3 Dragonfly Tail Mantis Head Beetle Tail Ant Head
  , makePiece 4 Dragonfly Tail Ant Head Mantis Head Ant Tail
  , makePiece 5 Dragonfly Tail Ant Head Beetle Head Mantis Head
  , makePiece 6 Dragonfly Head Beetle Tail Mantis Head Ant Tail
  , makePiece 7 Dragonfly Head Mantis Tail Beetle Head Ant Tail
  , makePiece 8 Dragonfly Head Mantis Head Beetle Head Dragonfly Tail
  , makePiece 9 Beetle Tail Mantis Tail Ant Head Beetle Head
  ])

makePiece :: Int -> Insect -> End -> Insect -> End -> Insect -> End -> Insect -> End -> Piece
makePiece n i1 e1 i2 e2 i3 e3 i4 e4 = Piece {
    name = show n, rotation = 0, top = e i1 e1, right = e i2 e2, bottom = e i3 e3, left = e i4 e4
  }
  where e x y = Edge { insect = x, end = y }

showBoard :: Board -> String
showBoard b = foldl (\acc p -> acc ++ showPiece p) "" b
  where showPiece p = name p ++ "," ++ (show . rotation $ p) ++ " "

solution :: State -> Board
solution (b, s)
  | length b == 9 = b
  | otherwise =
      head [ solution (nb, ns) | (nb, ns) <- nextBoards, boardIsLegal nb ]
  where nextBoards = foldr (addWithEachRotation b s) [] s

addWithEachRotation :: Board -> SetOfPieces -> Piece -> [State] -> [State]
addWithEachRotation b s p = (\acc -> acc)

-- Only check latest piece
boardIsLegal :: Board -> Bool
boardIsLegal b
  | pos < 2 = True
  | otherwise = (topRow || matchesAbove) && (leftCol || matchesLeft)
  where this = tail b !! 0
        pos = length b - 1
        topRow = pos < 3
        leftCol = pos `mod` 3 == 0
        matchesAbove = edgesMatch (top this) . bottom . fromJust . above b $ pos
        matchesLeft = edgesMatch (left this) . right . fromJust . leftTo b $ pos

above :: Board -> Int -> Maybe Piece
above = relativePiece (<3) 3

leftTo :: Board -> Int -> Maybe Piece
leftTo = relativePiece ((==) 0 . mod 3) 1

relativePiece :: (Int -> Bool) -> Int -> Board -> Int -> Maybe Piece
relativePiece cond offset b index
  | cond $ index = Nothing
  | otherwise = Just $ b !! (index - offset)

edgesMatch :: Edge -> Edge -> Bool
edgesMatch e1 e2 = (insect e1 == insect e2) && (end e1 /= end e2)
