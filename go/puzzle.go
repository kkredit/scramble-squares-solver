// puzzle.go
// Solves a scramble-squares puzzle using idiomatic Go
// Copyright (c) 2020, Kevin Kredit
// License MIT

package main

import (
	"errors"
	"fmt"
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

// Boards
type PlacedPiece struct {
	piece    Piece
	rotation int // Clockwise 90 deg rotations
}

type Position int

const (
	Top Position = iota
	Right
	Bottom
	Left
)

func (pp *PlacedPiece) getEdge(po Position) Edge {
	return pp.piece[(4+int(po)-pp.rotation)%4]
}

type PieceSet []Piece

type Board struct {
	placedPieces   []PlacedPiece
	unplacedPieces PieceSet
}

func (pieces *PieceSet) pop(index int) (Piece, error) {
	if index >= len(*pieces) {
		return Piece{}, errors.New("Index is out of range")
	}
	// Get desired piece, then cut it out of slice
	p := (*pieces)[index]
	*pieces = append((*pieces)[:index], (*pieces)[index+1:]...)
	return p, nil
}

func (b *Board) newCopy() Board {
	var c = Board{
		make([]PlacedPiece, len(b.placedPieces)),
		make(PieceSet, len(b.unplacedPieces)),
	}
	copy(c.placedPieces, b.placedPieces)
	copy(c.unplacedPieces, b.unplacedPieces)
	return c
}

func (b *Board) placePiece(index int, rot int) {
	if p, err := b.unplacedPieces.pop(index); err == nil {
		b.placedPieces = append(b.placedPieces, PlacedPiece{p, rot})
	} else {
		panic(err)
	}
}

func (b *Board) isValid() bool {
	// Assume all but latest piece have been checked
	index := len(b.placedPieces) - 1
	piece := b.placedPieces[index]
	if index%3 != 0 {
		// Check left edge
		leftPiece := b.placedPieces[index-1]
		if !piece.getEdge(Left).matches(leftPiece.getEdge(Right)) {
			return false
		}
	}
	if index >= 3 {
		// Check upper edge
		abovePiece := b.placedPieces[index-3]
		if !piece.getEdge(Top).matches(abovePiece.getEdge(Bottom)) {
			return false
		}
	}
	return true
}

func (b *Board) findSolution() {
	for index := range b.unplacedPieces {
		for rot := 0; rot < 4; rot++ {
			next := b.newCopy()
			next.placePiece(index, rot)
			if next.isValid() {
				if len(next.unplacedPieces) == 0 {
					fmt.Println("Found a solution!\n", next.placedPieces)
				} else {
					next.findSolution()
				}
			}
		}
	}
}

func main() {
	var board = Board{
		[]PlacedPiece{},
		PieceSet{
			Piece{-Dragonfly, Ant, -Beetle, Mantis},
			Piece{-Dragonfly, -Ant, Beetle, -Mantis},
			Piece{-Dragonfly, Mantis, -Beetle, Ant},
			Piece{-Dragonfly, Ant, Mantis, -Ant},
			Piece{-Dragonfly, Ant, Beetle, Mantis},
			Piece{Dragonfly, -Beetle, Mantis, -Ant},
			Piece{Dragonfly, -Mantis, Beetle, -Ant},
			Piece{Dragonfly, Mantis, Beetle, -Dragonfly},
			Piece{-Beetle, -Mantis, Ant, Beetle},
		},
	}

	fmt.Println("Working on a solution!")
	board.findSolution()
}
