# Scramble Squares Solver <!-- omit in toc -->

- [Puzzle](#puzzle)
- [Solutions](#solutions)
  - [C](#c)
  - [Go](#go)
  - [Haskell](#haskell)
  - [Rust](#rust)
- [Benchmarks](#benchmarks)

## Puzzle

In 2016 I spent Thanksgiving at a friend's house. They had a puzzle sitting on the table and it
looked simple--just arrange these pieces so all the shapes line up. I spent hours with it, and got
within one piece about ten different ways, but could never solve it. Time to write a program.

The puzzle is a "Scramble Squares" type of puzzle. You can find it
[here](https://www.puzzlewarehouse.com/Insects-10028ss.html) and
[here](https://www.amazon.com/B-Dazzle-10028-Scramble-Squares-Insects/dp/B000021Z0S). The idea is
that each touching edge must have a complete insect. For example, if you have the head of a beetle
pointing up on the left piece, you must have the abdomen of a beetle pointing down on the right
piece. Edges on the outer edge of the entire puzzle do not matter.

Here's a picture of what it looks like unsolved:

![image](images/puzzle.jpg)

## Solutions

This program was originally written in C. During an exploration of programming languages in 2020, I
added solutions in Go, Haskell, Rust, and Clojure. [Clojure TBD as of 3/24/20.]

Each implementation uses the same high-level algorithm. Data structures represent pieces as
arrangements of sides and boards as arrangements of pieces. The algorithm recursively places each
possible next piece in the next spot at each possible rotation. if that placement is illegal, it
stops recurring down that tree. When it has placed all nine pieces successfully, it has found a
solution. Solutions are printed out as (piece, rotation) tuples. The algorithm doesn't account for
rotations, so it finds four solutions.

### C

C is definitely not the simplest language for this problem. This implementation is not highly
optimized, and the board checking function in particular is quite bad.

### Go

The Go solution uses Goroutines to search for a solution in parallel. It was a fairly
straightforward port from C, though methods made the code simpler.

### Haskell

The Haskell solution is quite elegant. It is far simpler, though it runs slowly. I am not practiced
at writing efficient Haskell.

### Rust

The Rust solution feels like a mix of all the previous three languages. It's memory usage felt like
C. Of course, Rust is safe, but the way to think about memory felt the same. Rust's macro system,
similarly, is more powerful yet has a familiar feel. It's object methods felt like Go. The overall
structure of the program was extremely similar, and the LOC and Complexity metrics match almost
exactly. The pattern matching and functional-_lite_(TM) capability felt like Haskell. Programming
functionally in Rust is not natural, but having the capability when it is particularly convenient is
wonderful.

## Benchmarks

Note that the implementation in each language is relatively basic; none have been particularly
optimized for size or performance.

LOC and Complexity are reported by [scc](https://github.com/boyter/scc). The C application is built
with static linkage. The rest are with whatever is default.

The LOC and complexity comparisons are not entirely fair, as each program implements a slightly
different set of features. For example, the Go solution implements parallelism, which is trivial in
Go but would add considerable complexity to some other languages. The Rust solution implements
`fmt::Display for Board`, which alone accounts for 27 LOC and 7 points of complexity. For a fair
comparison, each language should implement the same features.

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | 10x Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|
| c        | 170   | 83         | 0.10           | 856           | 0.02            | 2924          |
| go       | 123   | 18         | 0.30           | 1982          | 0.44            | 7736          |
| haskell  | 71    | 6          | 0.82           | 1126          | 1.60            | 4296          |
| rust     | 125   | 18         | 0.49           | 2671          | 1.79            | 2852          |
