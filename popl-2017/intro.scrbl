#lang scribble/sigplan @onecolumn

@require["common.rkt" (only-in scribble/base nested)]

@title[#:tag "sec:intro"]{Type Eleborators Need APIs}

Type systems such as ML's, Haskell's, or Scala's help developers convey
their thinking to their successors in a sound and concise manner.  The
implementations of such type systems tend to employ elaboration passes to
check the types of a specific program. As an elaborator traverses the
(parsed) synatx, it simultaneously reconstructs types, checks their
consistency according to the underlying type theory, replaces many of the
surface-syntax constructs with constructs from the kernel language, and
inserts type information to create an annotated representation.

Some (implementations of such) programming languages also support a way to
programmatically direct the elaborator. For example, Rust and Scala come
with compiler plug-ins. Typed Racket and Types Clojure inherit the macro
mechanisms of their parent languages. Here we refer to such mechanisms as
@defterm{elaborator API}s. 

Equipping type-checking elaborators with APIs---or using the existing
APIs---promises new ways to expand the power of type checking at a
relatively low cost. Specifically, the developers of libraries can
@defterm{tailor} typing rules to the exported functions and linguistic
constructs.

Consider the example of tailoring the API of a library that implements a
string-based, domain-specific languages. Examples of such libraries abound:
formatting, regular-expression matching, database queries, and so on.  The
creators of these DSLs tend to know a lot about how programs written in
these DSLs relate to the host program, but they cannot express this
knowledge in API types without resorting to such rich systems as
dependent type theory.

Type tailoring allows such programmers to refine the typing rules for the
APIs in a relatively straightforward way. Recall the API of the
regular-expression library. In a typical typed language, the matching
function is exported with a type like this: 
@;
@verbatim[#:indent 4]{

reg_exp_match: String -> Option [Listof String]

}
@;
When the result is a list of strings, the length of the list depends on the
argument string---but the API for the regular-expression library cannot
express this relationship between the regular expression and the
surrounding host program. As a result, the author of the host program must
inject additional complexity, often in the form of (hidden) run-time checks.

If type tailoring is available, the creator of the library can refine the
type of @tt{reg_exp_match} with rules such as these:
@;
@verbatim[#:indent 4]{

   Program |- s : does not contain grouping
------------------------------------------------
  G |- reg_exp_match(s) : Option [List String]


Program |- s : contains one grouping w/o alternative 
-----------------------------------------------------
 G |- reg_exp_match(s) : Option [List String String]

}
@;
That is, when the extended elaborator can use (a fragment of) the program
to prove that the given string for a specific use of @tt{reg_exp_match}
does not contain grouping specifications---ways to extract a sub-string via
matching---then the @tt{Some} result type is a list that contains a single
string. Similarly, if the string contains exactly one such grouping (and no
alternative), then a match produces a list of two strings. In all other
cases, the type checker uses the default type for the function.

A vector library is another familiar example that offers opportunities for
type tailoring. Often such a library exports a dereferencing construct with
the following type rule: 
@;
@verbatim[#:indent 4]{

  G |- v : Vector[X]   G |-e : Integer
----------------------------------------
  G |- v[e] ~~> checked_ref(v,e) : X
}
@;
If the elaborator can prove, however, that the indexing expression @tt{e}
is equal to or evaluates to a specific integer @tt{i}, the rule can be
strengthened as follows: 
@;
@verbatim[#:indent 4]{

  Program |- e = i for some i in Natural
  && Program |- v is vector of length n
  && i < n

  G |- v : Vector[X]   G |-e : Integer
----------------------------------------
  G |- v[e] ~~> unsafe-ref(v,e) : X
}
@;
 That is, the elaborator can then eliminate a possibly expensive run-time
 check.

This paper demonstrates the idea with concrete case studies of type
tailoring (section 2) in the context of Typed Racket and its API to the
elaborator (section 3).  To illustrate the usefulness of the idea, we
implement two tailorings. The first one---chosen for the wide familiarity
of the domain---enriches the type-checking of vector-referencing operations
(section 4). The second example explains how the implementor of a
string-based embedded DSL---a regexp-matching DSL---may tailor the types of
the interpretation function (section 5). Our evaluations confirm that these
tailorings reduce the number of run-time checks that the programmer or the
elaborator have to insert into the host program. In addition, we sketch
several other applications of type tailoring in Typed Racket (section
6). We also explain how the creator of such libraries can refine the
existing type soundness proof of the host language to argue the correctness
of the tailoring.
