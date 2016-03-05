#lang scribble/sigplan @onecolumn

@; TODO need a word for these 'observable properties'
@; - regexp groups
@; - format characters
@; - procedure arity
@; - tuple size
@; - vector length
@; - database schema

@; Tuples in Haskell http://hackage.haskell.org/package/tuple
@; Regexp
@; - ocaml http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
@; - haskell https://hackage.haskell.org/package/regex-compat/docs/Text-Regex.html

@require["common.rkt"]


@title{Introduction}
@; tautology checker
@; curry
@; deep
@; regexp match

Many useful functions do not have simple types.
For example, one cannot write a general procedure for currying functions
 or accessing the first element of an arbitrarily-sized tuple in OCaml or Haskell.
@; TODO don't mention ML/Hask ... use System F?
Nevertheless, specialized versions of @racket[curry] and @racket[first] are
 easy to define and type.

@racketblock[
    > curry (λ (x y) x)           > first (x, y)
    (λ (x) (λ (y) x))             x

]

That is, once some basic structure of the input is fixed, the general
 problem becomes a much simpler, specific problem.
If ever a 16-argument function needs currying, we can write a new function for
 the task.

Regular expression patterns are another common value whose structure is not
 expressed by conventional type systems.
Consider the function @racket[regexp-match] from Typed Racket:

@interaction[
  (regexp-match #rx"types" "types@ccs.neu.edu")

  (regexp-match #rx"lambda" "types@ccs.neu.edu")

  (regexp-match #rx"(.*)@(.*)" "types@ccs.neu.edu")
]
@; TODO note that nested groups cannot fail.

When called with a pattern @racket[p] and string @racket[s] to match with,
 @racket[(regexp-match p s)] returns a list of all substrings in @racket[s]
 matching groups in the pattern @racket[p].
The pattern itself counts for one group and parentheses within @racket[p]
 delimit nested groups.
Hence the third example returns a list of three elements.
A match can also fail, in which case the value @racket[#f] is returned.

Although Typed Racket's @racket[regexp-match] has the type@note{Assuming that a
    successful match implies that all nested groups successfully match.}
 @racket[(Regexp String -> (U #f (Listof String)))], the number of strings
 in the result list is determined by the number of groups in the input regular
 expression.
Like the arity of a function or the size of a tuple, the number of groups
 in a pattern is often clear to the programmer.
We could imagine using
 an indexed family of @racket[regexp-match] functions for patterns of
 two, three, or more groups.
Ideally though, the programming language should understand
 these unconventional or domain-specific flavors of polymorphism.

This pearl describes a technique for extending a simple type system with
 a value-indexed form of polymorphism.
By analyzing the syntactic structure of values and partially evaluating
 constant expressions before typechecking, we specialize the types of functions
 like @racket[curry], @racket[first], and @racket[regexp-match] at their
 call-sites when possible.
Whereas the general type of @racket[curry] is @racket[(⊥ -> ⊤)],
 our system infers when it is safe to use a subtype instead.
For instance:

@racketblock[
 (curry (λ (x y) x))
]

generates the type constraint

@racketblock[
 curry : ((A B -> A) -> (A -> B -> A))
]

The technique does not require any changes to the underlying type system
 or annotations from the programmer.
Instead, we leverage existing tools for writing syntax extensions.
Our implementation happens to use Racket's macro system, but (at least)
 Clojure,
 Haskell,
 JavaScript,
 OCaml,
 Rust,
 and Scala
 are equally capable.

@section{Coming Attractions}

Section 2 describes our approach,
 Section 3 gives applications.
Section 4 presents the code
 and Section 5 reports on practical experience.
We conclude with related work and reflections.

