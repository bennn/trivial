#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:experience"]{Backwards Compatibility}

Our initial experience programming with the library has been positive.
By far the most useful application is to @racket[regexp-match], as
 Typed Racket's default requires either a type cast or guards on each
 matched group.
The bugs reported by @racket[printf] and others have also proven useful
 in development.

@;Regarding the optimized vector operations mentioned at the end of @Secref{sec:vector},
@; we have observed performance improvements on microbenchmarks but have yet
@; to find real programs where the same benefits apply.
@;In future work, we hope to improve the vector library to include matrix arithmetic
@; and try similar experiments on numeric code.

To gauge the applicability of our library to existing code,
 we have applied it to 10,000 lines of Typed Racket code
 taken from 7 small projects.
In total, we had to modify 6 lines to replace
 unrestricted mutation--which could violate our library's tagging assumptions---with
 reference cells.@note{None of the surveyed code used the database library;
  we have only tested that interface in scripts.}
This gives us confidence that retroactively opting-in to our library truly is a 1-line
 effort.
Using the library also enables the removal of casts and type annotations made
 redundant by our elaborations.

Compiling with our library adds no statistically significant overhead, but
 tends to produce slightly larger bytecode files due to the inserted
 annotations (at most 2% larger).
Running times we observed were on largely unaffected, but one project
 exhibited a 2-second slowdown due to added type casts.
This could be improved by a closer integration with the type checker to
 remove casts guaranteed to succeed.
Overall though, we find these performance characteristics encouraging.

@;@figure["fig:experience"
@;  "Experience Report"
@;  @exact|{\input{fig-experience}}|
@;]
