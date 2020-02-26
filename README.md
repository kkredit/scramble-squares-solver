# Scramble Squares Solver <!-- omit in toc -->

- [The Puzzle](#the-puzzle)
- [The Programs](#the-programs)
  - [C](#c)
  - [Go](#go)
  - [Haskell](#haskell)
- [Benchmarks](#benchmarks)

## The Puzzle

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

## The Programs

This program was originally written in C. During an exploration of programming languages in 2020, I
added solutions in Go, Haskell, Rust, and Clojure. [Still TBD as of 1/22/20.]

Though the C solution could be directly ported to other languages, the purpose of the exercise is to
use idiomatic code to demonstrate the power of each new language.

### C

The program represents pieces as arrangements of sides, then tries to place each piece in the next
spot at each possible rotation. As soon as it determines an illegal board position, it stops
searching down that path. When it has placed all nine pieces successfully, it has found a solution.
Solutions are printed out as (piece, rotation) tuples. It doesn't account for rotations, so
naturally it finds four solutions. The program could be easily adapted to solve any variant of this
type of puzzle.

C is definitely not the simplest language for this problem, but at the time it was my most
comfortable language.

### Go

The Go solution uses Goroutines to search for a solution in parallel.

### Haskell

The Haskell solution is quite elegant.

## Benchmarks

Note that the implementation in each language is relatively basic; none have been particularly
optimized for size or performance.

LOC and Complexity are reported by [scc](https://github.com/boyter/scc). The C application is built
with static linkage. The rest are with whatever is default.

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:-----------:|:-------------:|
| c        | 170   | 83         | 0.36           | 856           | 0.00        | 700           |
| go       | 123   | 18         | 0.82           | 1982          | 0.06        | 6700          |
| haskell  | 71    | 6          | 2.39           | 1126          | 0.60        | 4228          |
