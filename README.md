# Lisp

Lisp explained in a very simple way.

## What Lisp?

The original language Lisp went to hundreds of modifications which they were used to create the only Lisp we could find nowadays: Common Lisp and its minor sister Scheme.

The rules of Common Lisp are dictated by ANSI in the X3J13 specification. Every implementation which wants to be full compatible with Common Lisp should follow this rules.

Some examples of implementations are Steel Bank Common Lisp (SBCL), CLisp and more recently for Android devices, CL-REPL.

This document is about Common Lisp.

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

## Object-Oriented Programming in Common Lisp

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
(defgeneric init_point (point)
    (:documentation "You can add your explanation here"))

(defgeneric foo (point))
(defgeneric bar (point))
(defgeneric baz (point))
```

Observe that, in contrast with C++ where you define an argument with then name of the class first and then the name of the instance, in Common Lisp is viceversa.

To the "group" of functions related to a specific class in Common Lisp is called "the protocol of the class".

This is what we could call DEFGENERIC.

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
    (setf (slot-value 'p z) 0))
```

To this initialization function in Common Lisp is called the constructor of the class.

This is what we could call DEFMETHOD

## Example

```Lisp
(defclass point ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (z :initform 0 :initarg :z :accessor z)))

(defclass rect ()
  ((p1 :initform (make-instance 'point) :initarg :p1 :accessor p1)
   (p2 :initform (make-instance 'point) :initarg :p2 :accessor p2)))

(let ((r (make-instance 'rect)))
  (setf (x (p1 r)) 10)
  (setf (y (p1 r)) 10)
  (format t "Rect coords: (~d, ~d), (~d, ~d)~%" (x (p1 r)) (y (p1 r)) (x (p2 r)) (y (p2 r))))

(let ((r (make-instance 'rect :p2 (make-instance 'point :x 5 :y 5))))
  (setf (x (p1 r)) 10)
  (setf (y (p1 r)) 10)
  (format t "Rect coords: (~d, ~d), (~d, ~d)~%" (x (p1 r)) (y (p1 r)) (x (p2 r)) (y (p2 r))))

;;--------------------------------------------------------------------

(defgeneric x1 (rect))
(defgeneric y1 (rect))
(defgeneric x2 (rect))
(defgeneric y2 (rect))

(defmethod x1 ((r rect)) (x (p1 r)))
(defmethod y1 ((r rect)) (y (p1 r)))
(defmethod x2 ((r rect)) (x (p2 r)))
(defmethod y2 ((r rect)) (y (p2 r)))

(let ((r (make-instance 'rect :p2 (make-instance 'point :x 5 :y 5))))
  (setf (x (p1 r)) 10)
  (setf (y (p1 r)) 10)
  (format t "Rect coords: (~d, ~d), (~d, ~d)~%" (x1 r) (y1 r) (x2 r) (y2 r)))

;;--------------------------------------------------------------------

(defgeneric (setf x1) (value rect))
(defgeneric (setf y1) (value rect))
(defgeneric (setf x2) (value rect))
(defgeneric (setf y2) (value rect))

(defmethod (setf x1) (value (r rect)) (setf (x (p1 r)) value))
(defmethod (setf y1) (value (r rect)) (setf (y (p1 r)) value))
(defmethod (setf x2) (value (r rect)) (setf (x (p2 r)) value))
(defmethod (setf y2) (value (r rect)) (setf (y (p2 r)) value))

;;--------------------------------------------------------------------

(defgeneric imprimir (rect) (:method ((r rect)) (format t "*Rect coords: (~d, ~d), (~d, ~d)~%" (x1 r) (y1 r) (x2 r) (y2 r))))

(let ((r (make-instance 'rect)))
  (setf (x1 r) 3)
  (setf (y1 r) 3)
  (setf (x2 r) 8)
  (setf (y2 r) 8)
  (imprimir r)
  (format t "Rect coords: (~d, ~d), (~d, ~d)~%" (x1 r) (y1 r) (x2 r) (y2 r)))

;;--------------------------------------------------------------------

(defgeneric set-identity (rect value)
  (:method ((r rect) value)
    (setf (x1 r) value)
    (setf (y1 r) value)
    (setf (x2 r) value)
    (setf (y2 r) value)))

(let ((r (make-instance 'rect)))
  (set-identity r (- 3))
  (imprimir r))
```
