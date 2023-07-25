Assignment 1b: Monads + DPLL
============================

This assignment consists of two parts. First, you will learn how to use monads,
as these are needed for the later assignments. In the second part you will use
your Haskell skills to implement a SAT solver using the DPLL algorithm. As you
know, logic is the language of computation, and by solving logicical problems,
we'll be able to reason about the correctness of programs. In the final part of
the assignment, you will then extend the DPLL solver into a simple SAT solver
for the theory of linear rational arithmetic (LRA).

Getting started
===============

This assignment runs using stack and works like the last assignment.
Just clone this repository using ``git clone ...``
and run it using


    $ stack test

which will tell you your current grade. As before you can use 

    $ stack ghci

for debugging.

First Part: Monads
==================

This part of the assignment is similar to assignment 1a. The objective of this
part of the assignment is to teach you how to use Monads with both bind and do-notation.

As you will see, Monads are part of any serious Haskell program, and you will
need them to implent the concepts we've discussed in class.

The files for this part of the assignment are found in the ``src/monads``
folder. We recommend going through them in the following order:

1. ``src/monads/Maybe.hs``
2. ``src/monads/List.hs``
3. ``src/monads/Reader.hs``
4. ``src/monads/Writer.hs``
5. ``src/monads/State.hs``

Apart from introducing Monads, this part of the assignment will help you deepen
your Haskell programming skills. You'll need them later! What's more, a deeper
understanding of functional programming will make you a better programmer in any
langugage. 

Assignment: DPLL & SMT
=======================

In the second part of the assignment, you'll start implementing the concepts we
discussed in the first few lectures. You'll start by implementing a naive
conversion into CNF. Next, you'll implement Tseitin's transformation, and use it
to write a DPLL solver that implements the two optimizations we discussed in
class: Boolean constraint propagation (BCP) and pure literal elimination (PLP).
Finally, you'll use the DPLL solver to implement a simple SMT solver for the
theory of linear rational arithmetic (LRA).

As before, you'll be asked to complete function stubs in the files. This time
though, we will give you a skeleton that strings your stubs together. In the
end, you will have a functioning SAT and SMT solver that you can use to solve
arbitrary problems!! What a function is expected to do is again annotated above
it. You will need to go through the following steps to implement the SAT and SMT
solvers:

1. Implement CNF conversion in ``src/sat/Prop.hs``
2. Implement Tseitins Transformation in ``src/sat/Tseitin.hs``
3. Implement the DPLL algorithm ``src/sat/DPLL.hs``
4. Implement the interface with the LRA theory solver ``src/sat/LRA.hs``
5. Implement the SMT solver ``src/sat/SMT.hs``

Remember that you may individually run the functions via ``stack ghci``. Specifically,
there is a ``parse`` function available that allows you to easily specify propositional
formulas in a string, e.g.

.. code:: haskell

    ghci> cnf $ parse "x & -x | z"

Which will output your implementation of the cnf transformation.

The ``src/sat/CNF.hs`` file contains the rigid CNF datastructure, which is a
propositional type that can only ever be in CNF. Do make sure to read and
understand this structure, as it is the output of the Tseitin Transformation
and the main computational structure of the DPLL procedure.

Similarly, for the SMT part of the assignment you can use parsing function
``parseLRA`` to parse an LRA formula, e.g.

.. code:: haskell

    ghci> parseLRA "x>=0 & y>=4 & (x+y<=0 | x+(-y)=0)"

Running the SAT solver
----------------------
If you have a (semi) working product. You can run the following to run the SAT solver:


    $ stack run

Do provide it input!

Grading
=======

You can see your current grade, when running 


    $ stack test

Do not modify other code than the stubs you are asked to implement, and do not
change the tests. Attempts to change the test harness as well as plagiarism will
result in failing the assignment with 0 pts. Barring this, the displayed grade
will correspond to your final grade, so you will always know where you stand. 

If you have difficulties with the assignment, go to the TA sessions and get
help! You can also post questions to the discussion board, but do not share your
own solution there. Don't worry about asking ``dumb`` questions. As far as we are
concerned, these don't exist, and most likely someone else has the same problem
but they are afraid to ask. By asking, you're doing them a favor! Use the
resources offered in this course. We want to you to succeed, and we'll put in
the work to help you.

Try to write pretty Haskell. Haskell allows you to write beautiful and concise
code. Good solutions are almost always short. If your functions are becoming too
big, with many case-splis and special cases, take a step back and think if there
is a simpler way of doing it. 

You can use ``http://learnyouahaskell.com/chapters`` as a reference, if there's
some concepts you are unsure about. Unless stated otherwise, you're allowed to
use functions from the Haskell standard library
``https://hackage.haskell.org/package/base``. If you come across a function
you're not familiar with, or you're looking for a specific function and can
guess its name, try using ``https://hoogle.haskell.org/``, a search engine for
Haskell functions. You can search by name, or by type. 

Try writing idiomatic Haskell code. For example, here is a style guide you can
use as a reference: ``https://kowainik.github.io/posts/2019-02-06-style-guide``.
If you're using VSCode's Haskell extension, it will also give you refactoring
hints on how to make your code nicer.

We will show you our own reference solution in the TA sessions after the
deadline.

Last, have fun! You're going to write a functioning SAT and SMT solver on your
own! Isn't that great? We think there's no better way of understanding something
than by building it yourself. Once you've mastered this assignment, you'll
master any SAT and SMT questions that may come your way in the future.