# Lisp

Lisp explained in a very simple way.

## What Lisp?

The original language Lisp went to hundreds of modifications which they were used to create the only Lisp we could find nowadays: Common Lisp and its minor sister Scheme.

The rules of Common Lisp are dictated by ANSI in the X3J13 specification. Every implementation which wants to be full compatible with Common Lisp should follow this rules.

Some examples of implementations are Steel Bank Common Lisp (SBCL), CLisp and more recently for Android devices, CL-REPL.

## Lists

Everything in Lisp is a list:

`(foo bar baz)`

## Atoms

Lists are made of atoms (the indivisible part of Lisp):

`FOO`, 
`BAR`, 
`BAZ`.

Atoms are separated by space:

`THERE ARE FIVE ATOMS HERE`

In Lisp is legal to use the hyphen as the name of an atom.

`THIS-IS-A-VERY-LONG-NAME-FOR-AN-ATOM`, 
`THIS-IS-ANOTHER-DIFFERENT-ATOM`.

An atom can be:

* A string.
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
* Ratios: `1/2`, `3/5`, `5/8`, etc.

## Symbols

A symbol can be:

* A function name.
* A variable name.
* The name of a macro.
* A special operator.

## Cells

A symbol in a list is also known as a cell:

`(FOO)`

Lists can have as many cells as you want:

`(THIS LIST HAS A LOT A LOT A LOT OF CELLS)`

Even other lists:

`((BEER AND TACOS) OR (HAMBURGER WITH CHEEZE))`

## Cons cell

A cell in Lisp is known as a "cons cell".

## Variables

There are two types of variables:

* Lexical scoped (a.k.a. "local variables").
* Dynamic/Special scoped (a.k.a. "global variables").

## S-Expressions (a.k.a. Symbolic-Expressions)

All the "commands" in Lisp use the preffix notation:

Instead of writing `(3 + 4)`, you have to write `(+ 3 4)`.

Lisp does not EXECUTE COMMANDS, it EVALUATES EXPRESSIONS instead.

## Object-Oriented Programming with Common Lisp

Common Lisp uses a technology called CLOS for "Common Lisp Object System" which, from the C Programming Language point of view, it consists of a structure with functions related:


```C
typedef struct _POINT {
    int x;
    int y;
    int z;
} POINT;
```

That is what we would call in Common Lisp: DEFCLASS:

```Lisp
(defclass point ()
    ((x :accessor x :initform 0)
     (y :accessor y :initform 0)
     (z :accessor z :initform 0)))
```

The variable members of the class in Common Lisp are called "the slots" of the class.

To create "function members" in C, we could do:

```C
void init_point(POINT*);
void foo(POINT*);
void bar(POINT*);
```

In Common Lisp we must do:

```Lisp
(defgeneric init_point((p point)))
(defgeneric foo((p point)))
(defgeneric bar((p point)))
(defgeneric baz((p point)))
```

Observe that, in contrast with C++ where you define an argument with then name of the class first and then the name of the instance, in Common Lisp is viceversa.

To the "group" of functions related to a specific class in Common Lisp is called "the protocol of the class".

And finally we define the protocol of the class which in C would be something like:

```C
void init_point(POINT *p)
{
    p->x = 0;
    p->y = 0;
    p->z = 0;
}
```

In Common Lisp:

```Lisp
(defmethod init_point((p point))
    (setf (slot-value 'p x) 0)
    (setf (slot-value 'p y) 0)
    (setf (slot-value 'p z) 0)))
```

To this initialization function in Common Lisp is called the constructor of the class.
