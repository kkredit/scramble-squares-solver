
# Why

A couple years ago I spent Thanksgiving at a friend's house. They had this puzzle sitting on the
table and it looked pretty simple--just arrange these pieces so all the shapes line up. I spent
hours with it, and got within one piece about ten different ways, but could never solve it! But I
figured it wouldn't be hard for a computer...

# The Puzzle

The puzzle is a "Scramble Squares" type of puzzle. You can find it
[here](https://www.puzzlewarehouse.com/Insects-10028ss.html) and
[here](https://www.amazon.com/B-Dazzle-10028-Scramble-Squares-Insects/dp/B000021Z0S).

Here's a picture of what it looks like unsolved:

![image](images/puzzle.jpg)

# The Program

The program represents pieces as arrangements of sides, then tries to place each piece in the next
spot at each possible rotation. As soon as it determines an illegal board position, it stops
searching down that path. When it has placed all nine pieces successfully, it has found a solution.
Solutions are printed out as (piece, rotation) tuples. It doesn't account for rotations, so
naturally it finds four solutions. The program could be easily adapted to solve any variant of this
type of puzzle.

C is definitely not the simplest language for this problem, but at the time it was my most
comfortable language.
