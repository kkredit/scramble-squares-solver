// puzzle.rs
// Solves a scramble-squares puzzle using idiomatic Rust
// Copyright (c) 2020, Kevin Kredit
// License MIT

////////////////////////////////////////////////////////////////////// TYPES //
#[derive(Debug)]
enum End { Head, Tail }
use End::*;

#[derive(Debug)]
enum Insect { Ant, Beetle, Dragonfly, Mantis }
use Insect::*;

#[derive(Debug)]
struct Edge {
    insect: Insect,
    end: End
}

type Piece = [Edge; 4];

#[derive(Debug)]
struct PlacedPiece {
    index: u32,
    rotation: u32
}

type Board = Vec<PlacedPiece>;

////////////////////////////////////////////////////////////////////// MACROS //
macro_rules! make_piece {
    ($i1:expr, $e1:expr, $i2:expr, $e2:expr, $i3:expr, $e3:expr, $i4:expr, $e4:expr) => {
        [ Edge { insect: $i1, end: $e1 }, Edge { insect: $i2, end: $e2 },
          Edge { insect: $i3, end: $e3 }, Edge { insect: $i4, end: $e4 } ]
    }
}

////////////////////////////////////////////////////////////////////// DATA //
const SET_OF_PIECES: [Piece; 9] = [
    make_piece!(Dragonfly, Tail, Ant, Head, Beetle, Tail, Mantis, Head),
    make_piece!(Dragonfly, Tail, Ant, Tail, Beetle, Head, Mantis, Tail),
    make_piece!(Dragonfly, Tail, Mantis, Head, Beetle, Tail, Ant, Head),
    make_piece!(Dragonfly, Tail, Ant, Head, Mantis, Head, Ant, Tail),
    make_piece!(Dragonfly, Tail, Ant, Head, Beetle, Head, Mantis, Head),
    make_piece!(Dragonfly, Head, Beetle, Tail, Mantis, Head, Ant, Tail),
    make_piece!(Dragonfly, Head, Mantis, Tail, Beetle, Head, Ant, Tail),
    make_piece!(Dragonfly, Head, Mantis, Head, Beetle, Head, Dragonfly, Tail),
    make_piece!(Beetle, Tail, Mantis, Tail, Ant, Head, Beetle, Head)
];

////////////////////////////////////////////////////////////////////// FUNCTIONS //
fn main() {
    println!("Working on a solution! {:?}", SET_OF_PIECES);
    let _board = Board::new();
}
