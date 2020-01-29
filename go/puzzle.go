// puzzle.go
// Solves a scramble-squares puzzle using idiomatic Go
// Copyright (c) 2020, Kevin Kredit
// License MIT

package main

import (
	"fmt"
	"errors"
)

// Edges, pieces, and boards
type Edge int

func (e Edge) matches(e2 Edge) bool {
	return e == -e2
}

const (
	Ant Edge = 1 + iota
	Beetle
	Dragonfly
	Mantis
)

type Piece [4]Edge

// Piece defintions
var P1 = Piece{-Dragonfly, Ant, -Beetle, Mantis}
var P2 = Piece{-Dragonfly, -Ant, Beetle, -Mantis}
var P3 = Piece{-Dragonfly, Mantis, -Beetle, Ant}
var P4 = Piece{-Dragonfly, Ant, Mantis, -Ant}
var P5 = Piece{-Dragonfly, Ant, Beetle, Mantis}
var P6 = Piece{Dragonfly, -Beetle, Mantis, -Ant}
var P7 = Piece{Dragonfly, -Mantis, Beetle, -Ant}
var P8 = Piece{Dragonfly, Mantis, Beetle, -Dragonfly}
var P9 = Piece{-Beetle, -Mantis, Ant, Beetle}

// Boards
type PlacedPiece struct {
	piece    Piece
	rotation int	// Clockwise 90 deg rotations
}

type Position int

const (
	Top Position = iota
	Right
	Bottom
	Left
)

func (pp *PlacedPiece) getEdge(po Position) Edge {
	return pp.piece[(4 + int(po) - pp.rotation) % 4]
}

type PieceSet []Piece

type Board struct {
	placedPieces []PlacedPiece
	unplacedPieces PieceSet
}

func (pieces *PieceSet) pop(index int) (Piece, error) {
	if (index >= len(*pieces)) {
		return Piece{}, errors.New("Index is out of range")
	}
	fmt.Println("Popping:", index, "from Set:", (*pieces))
	// Get desired piece
	p := (*pieces)[index]
	// Swap back piece to index and truncate
	// (*pieces)[index] = (*pieces)[len(*pieces)-1]
	// *pieces = (*pieces)[:len(*pieces)-1]
	// *pieces = append((*pieces)[:index], (*pieces)[index+1:]...)
	copy((*pieces)[index:], (*pieces)[index+1:])
	(*pieces)[len(*pieces)-1] = Piece{}
	(*pieces) = (*pieces)[:len(*pieces)-1]
	fmt.Println("Set after:          ", (*pieces))
    return p, nil
}

func (b *Board) placePiece(index int, rot int) {
	if p, err := b.unplacedPieces.pop(index); err == nil {
		b.placedPieces = append(b.placedPieces, PlacedPiece{p, rot})
	} else {
		fmt.Println(err)
		panic(err)
	}
}

func (b *Board) isValid() bool {
	for index, piece := range b.placedPieces {
		if index % 3 != 0 {
			// Check left edge
			leftPiece := b.placedPieces[index - 1]
			if ! piece.getEdge(Left).matches(leftPiece.getEdge(Right)) {
				// fmt.Println("Board", b.placedPieces, "is invalid")
				// fmt.Println("index", index, "left (", piece.getEdge(Left))
				// fmt.Println("does not match right (", leftPiece.getEdge(Right))
				return false
			}
		}
		if index >= 3 {
			// Check upper edge
			abovePiece := b.placedPieces[index - 3]
			if ! piece.getEdge(Top).matches(abovePiece.getEdge(Bottom)) {
				// fmt.Println("Board", b.placedPieces, "is invalid")
				// fmt.Println("index", index, "top (", piece.getEdge(Top))
				// fmt.Println("does not match bottom (", abovePiece.getEdge(Bottom))
				return false
			}
		}
	}
	return true
}

// // Piece set
// var Set = Board{
// 	{}
// 	{P1, P2, P3, P4, P5, P6, P7, P8, P9}
// }

func (b *Board) findSolution() {
	for index := range b.unplacedPieces {
		for rot := 0; rot < 4; rot++ {
			next := *b
			next.placePiece(index, rot)
			if next.isValid() {
				if len(next.unplacedPieces) == 0 {
					fmt.Println("Found a solution!\n", next.placedPieces)
				} else {
					if len(next.unplacedPieces) < 5 {
						fmt.Println("Getting close,", len(next.unplacedPieces), "left")
						fmt.Println("Progress:\n\t", next.placedPieces)
					}
					next.findSolution()
				}
			}
		}
	}
}

func main() {
	a := []string{"A", "B", "C", "D", "E"}
i := 2

// Remove the element at index i from a.
copy(a[i:], a[i+1:]) // Shift a[i+1:] left one index.
a[len(a)-1] = ""     // Erase last element (write zero value).
a = a[:len(a)-1]     // Truncate slice.

fmt.Println(a) // [A B D E]

	var board = Board{
		[]PlacedPiece{},
		PieceSet{P1, P2, P3, P4, P5, P6, P7, P8, P9},
	}

	fmt.Println("Working on a solution!")
	board.findSolution()
}
