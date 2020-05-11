# The Right Tool for the Job--Learning Languages to Solve Problems

## Lessons from New Languages

Learning new languages has many benefits. Exploring languages teaches new concepts, reduces fear of the unknown, and
keeps the joy of programming alive.

Over the past several months I've undertaken the task of learning new languages by diving into Go, Haskell, Rust, and
Clojure. For each language, I read a little bit, followed introductory exercises, and then solved the same puzzle.
Studying these widely different languages gave me an appreciation for different approaches, and taught me three general
lessons.

First, opinionated design decisions can be good. Programming may be described as communication between a human and a
fancy electronic rock. There are inevitably many layers of abstraction, and with each layer comes the possibility of a
new structure or computational paradigm. At the language level, a few paradigms are procedural, object oriented,
functional, and declarative. Data can be mutable or immutable, multiprocessing can be managed several ways, and programs
can be precompiled or run-time interpreted. With all this variation comes complexity. Having a language make opinionated
choices about how it will handle these questions can reduce the complexity for the programmer.

One of the best ideas I discovered during this study is [waterbed theory](http://wiki.c2.com/?WaterbedTheory), which
states that "if you push down the complexity in one part of a language or tool, there is a compensation which increases
the complexity of another part of the language or tool." Leveraging a tool to solve technical problems frees you to
focus on the business logic. For example, Rust's memory model is more complex than C's, but its guarantees of memory
safety make it much easier to write correct programs.

Second, I like types. As a developer, runtime failures are my nemesis. Adding functionality is usually the easy part;
making sure it works in all cases, handles errors, and doesn't do what its not supposed to do is the hard part. Having
guarantees about these things at compile-time warms my heart. Functional languages help with this by reducing
boilerplate code and enforcing immutability. Strong types go even further by ensuring proper use of data structures,
providing memory safety (in the right type system), and preventing runtime exceptions at compile-time.

My frustration with Clojure affirmed my appreciation of types. Why am I getting null pointer errors when I switch
between lists and vectors? Why are my collections being implicitly cast all the time? How am I supposed to debug runtime
errors when I get a mile-high Java exception trace that makes no sense?? I know Clojure is a powerful and beloved
language, and my experience would have been immensely improved by having an experienced Lisper as a guide. Many enjoy
the freedom of weak types, but for me, the type system is a welcome place to store complexity.

Third, learning new languages makes me a better developer. Even though I don't use Go, Haskell, Rust, or Clojure on the
job, learning these languages has introduced me to new ideas that make my daily programming better. My development style
in all languages is purer. My Bash and Ruby are more functional, and my C is architected by splitting up calculations
and I/O. These practices make it easier to write concise, understandable, and testable code.

Just because I don't use these languages now doesn't mean I won't in the future. Using the right tool for the job
simplifies any engineering task. In our embedded work, DornerWorks may adopt Rust for its balance of modern features,
diverse native compilation support, and low-level yet safe memory management. In our web and IoT work DornerWorks may
use Go for its web-native approach with familiar feel, Haskell for its correctness, or Clojure for its extensibility.

## Hitting 1 Nail with 5 Hammers

Part of this study was writing the same puzzle solving program in each language. To really get a feel for a language or
tool, you have to step outside the tutorial and implement something non-trivial; this puzzle solver was my non-trivial
program.

The programs solve [Scramble Squares](https://www.scramblesquares.com/) puzzles. While small enough to fit in one file,
the program is big enough to force you to think about data structures, algorithms, and I/O. Scramble Squares puzzles
consist of nine square pieces that fit together to make a larger 3x3 square. In the solution, all interior edges must
match. The particular puzzle I solved was [called
"insects"](https://www.scramblesquares.com/shop/nature/insects-scramble-squares/). In this puzzle, edges contain either
the head or the tail of one of four insects. Here's a picture of what it looks like unsolved:

<div style="text-align:center">
  <img src="images/puzzle.jpg" width="500">
</div>

I originally wrote a solution in C when my primary motivation was solving the puzzle. During this programming languages
deep dive, I added solutions in Go, Haskell, Rust, and Clojure. I will describe each version briefly, but you can read
the source code and a full writeup [here](https://github.com/kkredit/scramble-squares-solver).

Each implementation uses the same high-level algorithm. Data structures represent boards as arrangements of pieces and
pieces as arrangements of sides. The algorithm places one piece at a time with each possible rotation. If the resulting
board position is valid, it recursively places the next piece. When it has placed all nine pieces successfully, it has
found a solution. Solutions are printed out as `(piece, rotation)` tuples. The algorithm doesn't account for rotations,
so it finds four solutions.

C is not the simplest language for this problem, but it is the language in which I have the most experience. It is
actually shorter than the Go and Rust solutions, but it also implements fewer features. There is less abstraction of the
problem space and it requires the programmer to do more of the mental lifting.

The Go solution was a fairly straightforward port from C, though easier management of dynamic objects (i.e., arrays) and
the ability to lay methods onto structs allowed for higher abstraction. The highlight of using Go was the simple
parallelization. After creating a single-threaded solution, going multi-threaded took only 10 minutes and 15 lines of
code!

The Haskell solution is quite elegant. It is far simpler, and runs in approximately the same time as the Go solution.
Since this was my first time diving into a purely functional language, adapting to the functional mindset was some work.
It helped that I was already using immutable data and recursion. I really appreciated Haskell's strong type system.

The Rust solution felt like a mix of the C, Go, and Haskell solutions. Its memory usage and macro system felt like C,
though decidedly more robust. It's object methods and overall program structure felt like Go; it was procedural with a
dash of OO, and allowed for a nice level of abstraction. Rust's functional-_lite_ features using iterators were a little
clunky, though still appreciated.

The Clojure solution is a direct port of the Haskell solution. I never quite grokked Clojure's REPL-oriented development
workflow. Despite the workflow being awkward and frustrating, the final product is very nice. The algorithm is identical
to Haskell's. The data types are essentially the same, though I couldn't explain how the data is actually stored at
various parts of the program. Unfortunately, I did not pick up enough of the language to appreciate its strong macro
system and concurrency features.

Solving this problem in new languages was a pleasure, and learning a new language is good for any developer. To that
end, I created a [new repository](https://github.com/kkredit/scramble-squares-solvers) to collect additional
implementations. Try out a new language, or improve on one of my implementations. Play around with it, or submit a PR if
you want. I'd love to see your solutions!

## Solving Problems for You

Keeping our skills sharp and finding the right tools for the job are important values to DornerWorks. Exercises like
this one, where we learn new languages, platforms, frameworks, and tools help us deliver quality solutions that fit your
needs. Like a good language manages complexity for the developer, can DornerWorks be the partner that manages technology
for you, so you can focus on _your_ customers? Schedule a meeting with us today and we will show you how.
