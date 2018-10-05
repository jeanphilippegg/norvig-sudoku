Sudoku Solver in Lisp
=====================

This is an implementation in Common Lisp of the sudoku solver described in [Norvig's essay](http://norvig.com/sudoku.html).

I tried translating Norvig's Python code in Lisp as closely as possible. Instead of dictionaries, I used alists to represent the map between a square and its possible values (and to represent all other maps).

Usage
-----

CL-USER> (display (solve "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"))

(The sudoku puzzle to solve is a string of 81 digits or dots.)
