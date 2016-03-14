#lang scribble/sigplan
@require["common.rkt"]

@; TODO experiment is BOGUS because it doesn't measure overhead of define/let
@; Make those automatic and try again!

@title[#:tag "sec:experience"]{Experience}

Our initial experience programming with the library has been positive.
By far the most useful application is to @racket[regexp-match], as
 Typed Racket's default requires either a type cast or guards on each
 matched group.
The bugs reported by @racket[printf] and others are also useful, though
 the real-time difference between catching a @racket[printf] bug during compilation
 versus finding the same bug at run-time is small, maybe 1-3 seconds.
We have also observed performance improvements on microbenchmarks involving
 maps over large vectors, though we have yet to find the same benefits in real
 code.

In total, we have applied the library to 10,000 lines of Typed Racket code
 taken from 7 small projects.
Including the library is a 1-line change, but the programmer needs to remove
 now-redundant type annotations and casts manually.
Compiling with our library adds no statistically significant overhead, but
 tends to produce slightly larger bytecode files (at most 2% larger).
Running times are slightly worse for the added casts; in the worst case
 we saw 2-second slowdown.
This could be improved by tighter integration with the type checker.


@;@figure["fig:experience"
@;  "Experience Report"
@;  @exact|{\input{fig-experience}}|
@;]
