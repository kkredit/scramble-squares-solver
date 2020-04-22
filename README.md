# Scramble Squares Solver <!-- omit in toc -->

A puzzle solving program written in a variety of languages.

- [Motivation](#motivation)
- [Solutions](#solutions)
  - [C](#c)
  - [Go](#go)
  - [Haskell](#haskell)
  - [Rust](#rust)
  - [Clojure](#clojure)
- [Benchmarks](#benchmarks)
  - [Puzzle](#puzzle)
  - [Baseline](#baseline)
  - [Difference: Baseline -> Puzzle](#difference-baseline---puzzle)

## Motivation

One holiday at a friend's house, I found a puzzle with a simple premise--arrange these nine pieces so all the edges line
up. I spent hours with it, and got within one piece of a solution at least ten different ways, but could not solve it.
Time to write a program.

The puzzle was of the "Scramble Squares" variety. You can find it
[here](https://www.puzzlewarehouse.com/Insects-10028ss.html) and
[here](https://www.amazon.com/B-Dazzle-10028-Scramble-Squares-Insects/dp/B000021Z0S). The idea is that each touching
edge must have a complete insect. For example, if you have the head of a beetle on the right edge of the left piece, you
must have the abdomen of a beetle on the left edge of the right piece. Edges on the perimeter of the puzzle do not
matter.

Here's a picture of what it looks like unsolved:

<div style="text-align:center">
  <img src="images/puzzle.jpg" width="500">
</div>

## Solutions

This program was originally written in C--_not_ because that's the best language for the problem, but because that was
my strongest language at the time. In order to get practice with new languages, I added solutions in Go, Haskell, Rust,
and Clojure.

Each implementation uses the same high-level algorithm. Data structures represent boards as arrangements of pieces and
pieces as arrangements of sides. The algorithm places one piece at a time with each possible rotation. If the resulting
board position is valid, it recursively places the next piece. When it has placed all nine pieces successfully, it has
found a solution. Solutions are printed out as `(piece, rotation)` tuples. The algorithm doesn't account for rotations,
so it finds four solutions.

What follows is a few notes on what makes each solution unique.

<!--
For blog post, create new repo designed to collect solutions!

- Update benchmark.sh to collect stats and name champions for each language
  - code golf
  - execution time
  - memory usage
- Maybe document all the algorithms employed
- Accept all solutions, but require standard based on benchmark.sh expectations (e.g. make)
- Probably standardize output so can automatically check correctness
  - As part of established rules
  - Probably need to make rules about libraries too. Only standard libraries? Only for printing and
      basic data types?
  - If competing for lines of code, must use standardized formatting tool?
-->

### C

C is not the simplest language for this problem, but it is the language in which I have the most experience. Though it
is actually shorter and less complex than the Go and Rust solutions, that is probably only because of my familiarity
with the language. There is less abstraction of the problem space and it requires the programmer to do more of the
mental lifting.

The memory usage is worth some discussion. I am pleased that my intuition guided me to make copies of the board and use
recursion. Since C programs often operate in memory constrained environments, a C developer may be tempted to modify a
single board structure in place using a traditional loop. However, mutation is easy to get wrong (as Rust makes clear),
and recursion lends itself to simpler code. Since the algorithm uses depth-first search and peaks at nine layers, there
is a maximum of nine boards and stack frames present at any time, making the actual memory cost both bounded and low.

### Go

The Go solution was a fairly straightforward port from C, though easier management of dynamic objects (i.e., arrays) and
the ability to lay methods onto structs allowed for higher abstraction. Though I did appreciate simpler memory
management, the lack of a deep copy ability in the standard libraries was annoying. Using nine lines of the program to
make a copy of the board struct seems like a waste.

Parallelization was the highlight of the Go solution. Even though the search algorithm is trivially parallizable--thanks
in part again to making board copies instead of mutating state--implementing threads in C would have been an
undertaking. In Go, once I had the single threaded solution, I had it parallelized in 10 minutes and with the addition
of only 15 lines of code! (See the commit
[here](https://github.com/kkredit/scramble-squares-solver/commit/f150b0701489af9bce75ea14367c53968f05b509).)

### Haskell

The Haskell solution is quite elegant. It is far simpler, and runs in approximately the same time as the Go solution.
Since this was my first time diving into a purely functional language (though perhaps I should say "primarily"
functional language), adapting to the functional mindset was some work. [_Learn You a
Haskell_](http://learnyouahaskell.com/) was an excellent resource. It helped that I was already using immutable data and
recursion. I also appreciated Haskell's strong types. They make it clear when you're doing something wrong, and in a few
cases, type signatures even helped me infer Haskell syntax.

An interesting point about the Haskell solution is that I accidentally wound up with a breadth-first search. The
algorithm is to map next possible states over the set of unplaced peices, filter on legal board positions, and recur. I
struggle to think of exactly how to implement a depth-first search in Haskell. Developing that algorithm would be a good
exercise.

### Rust

The Rust solution felt like a mix of the C, Go, and Haskell solutions. Its memory usage and macro system felt like C,
though decidedly more robust. It's object methods and overall program structure felt like Go; it was procedural with a
dash of OO, and allowed for a nice level of abstraction. Rust's functional-_lite_ features using iterators were a little
clunky, though still appreciated. Though I didn't use pattern matching, it is nice to have.

A nice feature of the Rust implementation is its solution printing. I decided to implement the `fmt::Display` trait,
since it seemed like the Rust-y thing to do. This added several lines of code, but got me to use the macro system and
resulted in the nicest printout of any language.

### Clojure

The Clojure solution is a direct port of the Haskell solution. I never quite grokked Clojure's REPL-oriented development
workflow. Despite the workflow being awkard and frustrating, the final product is very nice. The algorithm is identical
to Haskell's. The data types are essentially the same, though Clojure's internal translation between collection types
("seqable" types) meant that I'm not sure how the data is actually stored at various parts of the program.
Frustratingly, changing between a list and a vector can cause segementation faults (even though they all get converted
to sequences during functional operations?). Unfortunately, I did not pick up enough of the language to appreciate its
strong macro system and concurrency features.

To develop well with Clojure, as with any Lisp, requires a particular development setup. With no other language has an
[autoformatter](https://github.com/weavejester/cljfmt) been so necessary. I didn't quite achieve the proper workflow,
but I couldn't have written this program without VSCode's [Clojure
plugin](https://marketplace.visualstudio.com/items?itemName=avli.clojure) or this lifesaving [bracket colorizing
tool](https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer-2).

## Benchmarks

Note that the implementation in each language is relatively basic; none have been particularly optimized for size or
performance. LOC and Complexity are reported by [scc](https://github.com/boyter/scc). The C application is built with
static linkage. The rest are with whatever is default.

The LOC and complexity comparisons are not entirely fair, as each program implements a slightly different set of
features. For example, the Go solution implements parallelism, which is trivial in Go but would add considerable
complexity to some other languages. The Rust solution implements nicely-formatted solution print via `fmt::Display for
Board`, which alone accounts for 27 LOC and 7 points of complexity. For a fairer comparison, each language should
implement the same features. Consider also from the [scc
README](https://github.com/boyter/scc/blob/850e8be775dac636f9da5864b26974b123269bd2/README.md):

>The complexity estimate is really just a number that is only comparable to files in the same language. It should not be
used to compare languages directly without weighting them. The reason for this is that its calculated by looking for
branch and loop statements in the code and incrementing a counter for that file.  
Because some languages don't have loops and instead use recursion they can have a lower complexity count. Does this mean
they are less complex? Probably not, but the tool cannot see this because it does not build an AST of the code as it
only scans through it.

The tool also fails to measure the complexity of the Clojure solution, which really should be about the same as the
Haskell solution.

There are three sets of benchmarks: the [Puzzle](#puzzle) benchmarks run the puzzle solving applications. The
[Baseline](#baseline) benchmarks run a dummy application that performs each language's version of `return 0;`. The
[Difference](#difference-baseline---puzzle) benchmarks are calculated as `Puzzle - Baseline`. The purpose of these
separate sets is to identify what portion of each metric is due to the application vs due to the languages and toolsets
themselves.

### Puzzle

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | 10x Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|
| c        | 100   | 15         | 0.11           | 825           | 0.01            | 2980          |
| go       | 123   | 18         | 0.26           | 1416          | 0.44            | 7572          |
| haskell  | 70    | 6          | 1.13           | 1071          | 0.46            | 4012          |
| rust     | 125   | 18         | 0.95           | 2578          | 0.13            | 2868          |
| clojure  | 58    | 0          | 7.55           | 3629          | 32.97           | 255240        |

### Baseline

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | 10x Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|
| c        | 5     | 0          | 0.08           | 824           | 0.00            | 2916          |
| go       | 4     | 0          | 0.14           | 788           | 0.01            | 2824          |
| haskell  | 4     | 0          | 0.63           | 969           | 0.01            | 3380          |
| rust     | 3     | 0          | 0.20           | 2557          | 0.00            | 2792          |
| clojure  | 7     | 0          | 7.35           | 3602          | 16.95           | 95672         |

### Difference: Baseline -> Puzzle

| Language |  LOC  | Complexity | Build time (s) | Exe Size (KB) | 10x Runtime (s) | Mem: RSS (KB) |
|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|
| c        | 95    | 15         | 0.03           | 1             | 0.01            | 64            |
| go       | 119   | 18         | 0.12           | 628           | 0.43            | 4748          |
| haskell  | 66    | 6          | 0.50           | 102           | 0.45            | 632           |
| rust     | 122   | 18         | 0.75           | 21            | 0.13            | 76            |
| clojure  | 51    | 0          | 0.20           | 27            | 16.02           | 159568        |
