#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:conclusion"]{Closing the Books}

This pearl described a class of macros called @emph{textualist elaborators}
 designed to enhance the analysis of an existing type system without any
 input from programmers.
The main idea is to interpret values from the source code of a program
 and elaborate the same code into a form that helps the type system see what is
 obvious to a human reader.
From an engineering perspective, we used tools provided by Racket's macro
 system to enable local, compositional, and lexically-scoped reasoning
 when designing new elaborators.

A key hypothesis of this pearl is that our framework could be implemented in
 a variety of languages using their existing type and syntax extension systems.
Typed Clojure's macros@~cite[clojure-macros],
 Rust's compiler plugins@~cite[rust-compiler-plugins],
 and Scala's LMS framework@~cite[ramho-hosc-2013]
 seem especially well-suited to the task.
Template Haskell@~cite[spj-haskell-2002] and OCaml ppx@note{@url["http://whitequark.org/blog/2014/04/16/a-guide-to-extension-points-in-ocaml/"]} could
 also reproduce the core ideas, albeit
 after the programmer modifies function call-sites.

Whereas types support reasoning independent of data representations,
 we have argued that at least the source code details of values
 are worth taking into account during type checking.
Doing so will catch more errors and enable unconventional forms of polymorphism,
 all without changes to the language's type system or the programmer's code.

