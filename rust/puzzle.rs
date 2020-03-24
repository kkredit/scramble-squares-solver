// puzzle.rs
// Solves a scramble-squares puzzle using idiomatic Rust
// Copyright (c) 2020, Kevin Kredit
// License MIT

use std::fmt;

////////////////////////////////////////////////////////////////////// TYPES //
#[derive(Debug, Clone, PartialEq)]
enum End { Head, Tail }
use End::*;

#[derive(Debug, Clone, PartialEq)]
enum Insect { Ant, Beetle, Dragonfly, Mantis }
use Insect::*;

#[derive(Debug, Clone)]
struct Edge {
    insect: Insect,
    end: End
}

enum Side { Top = 0, Right, Bottom, Left }

type Piece = [Edge; 4];

#[derive(Debug, Clone)]
struct PlacedPiece {
    index: usize,
    rotation: usize
}

/* Board
 *    1 2 3
 *    4 5 6
 *    7 8 9
 */
#[derive(Debug, Clone)]
struct Board {
    placed: Vec<PlacedPiece>,
    unplaced: Vec<usize>
}

////////////////////////////////////////////////////////////////////// MACROS //
macro_rules! make_piece {
    ($i1:expr, $e1:expr, $i2:expr, $e2:expr, $i3:expr, $e3:expr, $i4:expr, $e4:expr) => {
        [ Edge { insect: $i1, end: $e1 }, Edge { insect: $i2, end: $e2 },
          Edge { insect: $i3, end: $e3 }, Edge { insect: $i4, end: $e4 } ]
    }
}

macro_rules! return_on_err {
    ($e:expr) => {
        let res = $e;
        if res.is_err() {
            return res
        }
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

////////////////////////////////////////////////////////////////////// METHODS //
impl Edge {
    fn matches(&self, other: &Edge) -> bool {
        self.insect == other.insect && self.end != other.end
    }
}

impl PlacedPiece {
    fn side(&self, s: Side) -> &Edge {
        &SET_OF_PIECES[self.index][((s as usize) + 4 - self.rotation) % 4]
    }

    fn top(&self) -> &Edge { self.side(Side::Top) }
    fn right(&self) -> &Edge { self.side(Side::Right) }
    fn bottom(&self) -> &Edge { self.side(Side::Bottom) }
    fn left(&self) -> &Edge { self.side(Side::Left) }
}

impl Board {
    fn new() -> Board {
        Board {
            placed: Vec::<PlacedPiece>::new(),
            unplaced: (0..9).collect()
        }
    }

    fn get_solutions(&self) -> Vec<Board> {
        if self.unplaced.is_empty() {
            println!("Solution! {:?}", self);
            let cp = self.to_owned();
            return vec![cp];
        }
        let mut solutions = Vec::<Board>::new();
        for p in 0..self.unplaced.len() {
            for r in 0..4 {
                let nb = self.place_piece(p, r);
                if nb.is_valid() {
                    if nb.unplaced.len() < 3 {
                        println!("Going deeper! {}", nb);
                    }
                    solutions.append(&mut nb.get_solutions());
                }
            }
        }
        solutions
    }

    fn place_piece(&self, p: usize, r: usize) -> Board {
        let mut new_placed = self.placed.to_vec();
        let mut new_unplaced = self.unplaced.to_vec();

        new_placed.push(PlacedPiece { index: p, rotation: r });
        new_unplaced.remove(p);

        Board {
            placed: new_placed.to_owned(),
            unplaced: new_unplaced.to_owned()
        }
    }

    fn is_valid(&self) -> bool {
        // Only check latest placed piece
        let cur = self.placed.last().unwrap();
        let index = self.placed.len() - 1;
        let is_top = index < 3;
        let is_left = index % 3 == 0;
        (is_top || (self.placed[index - 3].bottom().matches(cur.top()))) &&
            (is_left || (self.placed[index - 1].right().matches(cur.left())))
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut printed = 0;
        for pp in self.placed.iter() {
            if printed % 3 == 0 {
                return_on_err!(write!(f, "\n "));
            }
            return_on_err!(write!(f, " {}:{}", pp.index, pp.rotation));
            printed += 1;
        }
        return_on_err!(write!(f, "\n ->"));
        for up in self.unplaced.iter() {
            return_on_err!(write!(f, " {}", up));
        }
        writeln!(f, "")
    }
}

////////////////////////////////////////////////////////////////////// FUNCTIONS //
fn main() {
    println!("Working on a solution...");

    let solutions = Board::new().get_solutions();
    println!("Found {} solutions!", solutions.len());
    for b in solutions {
        println!("{}\n", b);
    }
}
