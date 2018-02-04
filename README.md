# Lisp

Lisp explained in a very simple way.

## Lists

Everything in Lisp is a list:

`(foo bar baz)`

## Atoms

Lists are made of atoms (the indivisible part of Lisp):

`FOO`, 
`BAR`, 
`BAZ`.

Atomos are separated by space:

`THERE ARE FIVE ATOMS HERE`

In Lisp is legal to use the hyphen as the name of an atom.

`THIS-IS-A-VERY-LONG-NAME-FOR-AN-ATOM`, 
`THIS-IS-ANOTHER-DIFFERENT-ATOM`.

An atom can be:

* A sting.
* A number.
* A symbol.

## Strings

Strings are surrounded with double quotes:

`"The quick brown fox jumps over the lazy dog"`, 
`"All the base belongs to us"`.

## Numbers

There are three different types of numbers:

* Integers: `1`, `23434`, `3434342`, etc.
* Floating-points: `3.141516`, `0.232`, `190.2323`, etc.
* Fractions: `1/2`, `3/5`, `5/8`, etc.

## Symbols

A symbol can be:

* A function name.
* A variable name.
* The name of a macro.
* A special operator.

## Variables

There are two types of variables:

* Lexical scoped (a.k.a. "local variables").
* Dynamic/Special scoped (a.k.a. "global variables").

## S-Expressions (a.k.a. Symbolic-Expressions)

All the "commands" in Lips use the preffix notation:

Instead of `(3 + 4)` you should write `(+ 3 4)`

Lisp does not EXECUTE COMMANDS, it EVALUATES EXPRESSIONS instead.
