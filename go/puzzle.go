// puzzle.go
// Solves a scramble-squares puzzle using idiomatic Go
// Copyright (c) 2020, Kevin Kredit
// License MIT

package main

import (
	"errors"
	"fmt"
	"sync"
)

////////////////////////////////////////////////////////////////////////////////
//																		Types //
type edge int
type piece [4]edge
type pieceSet []piece
type placedPiece struct {
	piece    piece
	rotation int // Clockwise 90 deg rotations
}
type board struct {
	placedPieces   []placedPiece
	unplacedPieces pieceSet
}
type position int

////////////////////////////////////////////////////////////////////////////////
//																	   Consts //
const (
	ant edge = 1 + iota
	beetle
	dragonfly
	mantis
)

const (
	top position = iota
	right
	bottom
	left
)

////////////////////////////////////////////////////////////////////////////////
//																    Functions //
func main() {
	var board = board{
		[]placedPiece{},
		pieceSet{
			piece{-dragonfly, ant, -beetle, mantis},
			piece{-dragonfly, -ant, beetle, -mantis},
			piece{-dragonfly, mantis, -beetle, ant},
			piece{-dragonfly, ant, mantis, -ant},
			piece{-dragonfly, ant, beetle, mantis},
			piece{dragonfly, -beetle, mantis, -ant},
			piece{dragonfly, -mantis, beetle, -ant},
			piece{dragonfly, mantis, beetle, -dragonfly},
			piece{-beetle, -mantis, ant, beetle},
		},
	}

	fmt.Println("Working on a solution!")

	var wg sync.WaitGroup
	for i := range board.unplacedPieces {
		wg.Add(1)
		go board.startWithIndex(i, &wg)
	}

	wg.Wait()
}

////////////////////////////////////////////////////////////////////////////////
//																      Methods //

// Methods on boards
func (b *board) startWithIndex(index int, wg *sync.WaitGroup) {
	defer wg.Done()

	for rot := 0; rot < 4; rot++ {
		next := b.newCopy()
		next.placePiece(index, rot)
		next.findSolution(index)
	}
}

func (b *board) findSolution(initIndex int) {
	for index := range b.unplacedPieces {
		for rot := 0; rot < 4; rot++ {
			next := b.newCopy()
			next.placePiece(index, rot)
			if next.isValid() {
				if len(next.unplacedPieces) == 0 {
					fmt.Println("Thread", initIndex, "found a solution!\n", next.placedPieces)
				} else {
					next.findSolution(initIndex)
				}
			}
		}
	}
}

func (b *board) newCopy() board {
	var c = board{
		make([]placedPiece, len(b.placedPieces)),
		make(pieceSet, len(b.unplacedPieces)),
	}
	copy(c.placedPieces, b.placedPieces)
	copy(c.unplacedPieces, b.unplacedPieces)
	return c
}

func (b *board) placePiece(index int, rot int) {
	if p, err := b.unplacedPieces.pop(index); err == nil {
		b.placedPieces = append(b.placedPieces, placedPiece{p, rot})
	} else {
		panic(err)
	}
}

func (b *board) isValid() bool {
	// Assume all but latest piece have been checked
	index := len(b.placedPieces) - 1
	piece := b.placedPieces[index]
	if index%3 != 0 {
		// Check left edge
		leftPiece := b.placedPieces[index-1]
		if !piece.getedge(left).matches(leftPiece.getedge(right)) {
			return false
		}
	}
	if index >= 3 {
		// Check upper edge
		abovePiece := b.placedPieces[index-3]
		if !piece.getedge(top).matches(abovePiece.getedge(bottom)) {
			return false
		}
	}
	return true
}

// Methods on pieceSets
func (pieces *pieceSet) pop(index int) (piece, error) {
	if index >= len(*pieces) {
		return piece{}, errors.New("Index is out of range")
	}
	// Get desired piece, then cut it out of slice
	p := (*pieces)[index]
	*pieces = append((*pieces)[:index], (*pieces)[index+1:]...)
	return p, nil
}

// Methods on placedPieces
func (pp *placedPiece) getedge(po position) edge {
	return pp.piece[(4+int(po)-pp.rotation)%4]
}

// Methods on edges
func (e edge) matches(e2 edge) bool {
	return e == -e2
}
