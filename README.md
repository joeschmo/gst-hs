# GODEL'S SYSTEM T

This project is a basic interpreter for the
[Godel's System T](http://en.wikipedia.org/wiki/Dialectica_interpretation) programming language.
It currently only suppports the execution of one-liners. Future updates will include better
error handling, global variables, and file IO support.

For a technical understanding of the semantics of Godel's System T, read Chapter 9 of 
[Bob Harper's book](www.cs.cmu.edu/~rwh/plbook/book.pdf).

## Syntax

Godel's System T only has two types: nat and function. There are only a couple expressions available:

The natural number zero: `z`

The successor "function": `s(e)`

Variables: `x`, `y`, `z`, `var`, `hello`, ...

Lambda abstraction: `fn (x : t) e`

Primitive recursion: `natrec e {z => e0 | s(x) with y => e1}`

Function application: `e(e')`

## Compiling

These instructions are for the linux and OS X operating systems. Go to [www.haskell.org](www.haskell.org)
to find instructions about compiling haskell programs on Windows.

Compiling requires the installation of [GHC](www.haskell.org/ghc/).

To compile, use the command `ghc --make Main.hs`. If GHC complains about the ambiguity
of Control.Monad.Error, then use the command `ghc --make -hide-package monads-fd Main.hs`.


## ToDo

- Implement File IO to allow the loading of *.gst files into the interpreter
