# GODEL'S SYSTEM T INTERPRETER

This project is a basic interpreter for the
[Godel's System T](http://en.wikipedia.org/wiki/Dialectica_interpretation) programming language.
It currently only suppports the execution of one-liners. The interpreter was written in the
haskell programming language.

For a technical understanding of the semantics of Godel's System T, read Chapter 9 of 
[Bob Harper's book](www.cs.cmu.edu/~rwh/plbook/book.pdf). For a more basic tutorial, go to the
section **Basic Tutorial to Godel's System T**.

## Compiling

These instructions are for the linux and OS X operating systems. Go to [www.haskell.org](www.haskell.org)
to find instructions about compiling haskell programs on Windows.

Compiling requires the installation of [GHC](www.haskell.org/ghc/).

To compile, use the command `ghc --make Main.hs`. If GHC complains about the ambiguity
of Control.Monad.Error, then use the command `ghc --make -hide-package monads-fd Main.hs`.

## Introduction to the Godel's System T Interpreter

Godel's System T can really only be used to do arithmetical operations. However, the expressitivity
of primitve recursion allows for most arithmetic operations to be codeable in this language.

### Syntax

Godel's System T only has two types: nat and function. There are only a couple expressions available:

The natural number zero: `z`

The successor "function": `s(e)`

Variables: `x`, `y`, `z`, `var`, `hello`, ...

Lambda abstraction: `fn (x : t) e`

Primitive recursion: `natrec e {z => e0 | s(x) with y => e1}`

Function application: `e(e')`

### Variable Assignment

This interpreter extends Godel's System T by adding in variable assignment.
This means that we can assign the expression `exp` to variable `x` via the command `x = exp`.
Variables assigned in this manner are in fact mutable, so the command `x = exp2` _will_ overwrite
the variable `x` with the expression `exp2`.

### Source File Loading

The interpreter also supports the loading of source files. The command `load "filename"` will
load the file with the given filename. The interpreter executes each line in the file one-by-one and
remembers variable assignments done in the source file. While any filename will do, it is preferred
that the filename ends with the .gst extension.

## Known Bugs

- do not have variable names that start with the letter `z`. Unfortunately the parser will consume
  the `z` and believed it has found the expression for zero.

## ToDo

- Perhaps a tutorial?
