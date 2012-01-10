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

## ToDo

- Refactor error handling in GstEval.hs to use MonadError

- Implement a global variable environment using Data.IORef

- Implement File IO to allow the loading of *.gst files into the interpreter
