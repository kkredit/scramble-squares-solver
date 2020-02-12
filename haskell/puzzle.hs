-- puzzle.hs
-- Solves a scramble-squares puzzle using idiomatic Haskell
-- Copyright (c) 2020, Kevin Kredit
-- License MIT

module Main
  where

main :: IO ()
main = print findSolution

data End = Head | Tail deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Insect = Ant | Beetle | Dragonfly | Mantis deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Edge = Edge { insect :: Insect, end :: End } deriving (Eq, Show, Read)
-- data Piece = TODO: vectors?

findSolution :: String
findSolution = "Working on it"
