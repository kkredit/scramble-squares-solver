# Scramble Squares Solver <!-- omit in toc -->

- [Puzzle](#puzzle)
- [Solutions](#solutions)
  - [C](#c)
  - [Go](#go)
  - [Haskell](#haskell)
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
added solutions in Go, Haskell, Rust, and Clojure. [Rust and Clojure TBD as of 3/7/20.]

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

## Benchmarks

Note that the implementation in each language is relatively basic; none have been particularly
optimized for size or performance.

LOC and Complexity are reported by [scc](https://github.com/boyter/scc). The C application is built
with static linkage. The rest are with whatever is default.

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | 10x Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|
| c        | 170   | 83         | 0.33           | 856           | 0.07            | 3000          |
| go       | 123   | 18         | 1.05           | 1982          | 0.94            | 6844          |
| haskell  | 71    | 6          | 2.43           | 1126          | 3.23            | 4324          |
