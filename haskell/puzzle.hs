-- puzzle.hs
-- Solves a scramble-squares puzzle using idiomatic Haskell
-- Copyright (c) 2020, Kevin Kredit
-- License MIT

module Main
  where

import Data.Maybe

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

main :: IO ()
main = putStrLn $ "Solution: " ++ show findSolution

showBoard :: Board -> String
showBoard b = foldl (\acc p -> acc ++ showPiece p) "" b
  where showPiece p = name p ++ "," ++ (show . rotation $ p) ++ " "

findSolution :: Board
findSolution = head . solutions $ ([], [
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

solutions :: State -> [Board]
solutions (b, []) = [b]
solutions (b, s) = [ (nb, ns) | (nb, ns) <- nextStates, boardIsLegal nb ] >>= solutions
  where nextStates = foldr (addWithEachRotation b s) [] s

addWithEachRotation :: Board -> SetOfPieces -> Piece -> [State] -> [State]
addWithEachRotation b s p = (++) newStates
  where newStates = [ ns | ns <- map nsWithRotation [1..4] ]
        nsWithRotation n = (b ++ (spunPiece n), filter (/= p) s)
        spunPiece n = tail . take n . iterate rotatePiece $ p

rotatePiece :: Piece -> Piece
rotatePiece p = Piece {
    name = name p, rotation = (rotation p + 1) `mod` 4
  , top = left p, right = top p, bottom = right p, left = bottom p
  }

-- Only check latest piece
boardIsLegal :: Board -> Bool
boardIsLegal b
  | pos < 1 = True
  | otherwise = (topRow || matchesAbove) && (leftCol || matchesLeft)
  where pos = length b - 1
        topRow = pos < 3
        leftCol = pos `mod` 3 == 0
        this = tail b !! 0
        matchesAbove = edgesMatch (top this) . bottom . fromJust . above b $ pos
        matchesLeft = edgesMatch (left this) . right . fromJust . leftTo b $ pos
        edgesMatch e1 e2 = (insect e1 == insect e2) && (end e1 /= end e2)
        above = relativePiece (<3) 3
        leftTo = relativePiece (\x -> x`mod`3==0) 1

relativePiece :: (Int -> Bool) -> Int -> Board -> Int -> Maybe Piece
relativePiece cond offset b index
  | cond $ index = Nothing
  | offset > index = Nothing
  | otherwise = Just $ b !! (index - offset)
