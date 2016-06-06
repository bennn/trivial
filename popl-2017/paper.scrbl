#lang scribble/sigplan @onecolumn @preprint

@(require "common.rkt")

@title{Tailoring Type Theories (T.T.T)}
@authorinfo["Piet Hein" "Funen, Denmark" "gruk at piethein.com"]

@abstract{

Many typed APIs implicitly acknowledge the @emph{diktat} that the host type
 system imposes on the creators of libraries. When a library implements a
 string-based domain-specific language, the problem is particularly obvious.
 The interpretation functions for the programs in this embedded language
 come with the uninformative type that maps a string to some other host
 type. Only dependently typed languages can improve on this scenario at the
 moment, but they impose a steep learning curve on programmers. 

This paper proposes to tackle this problem with APIs for type
 checkers. Specifically, it observes that most typed languages already
 employ an elaboration pass to type-check programs. If this elaborator
 comes with a sufficiently rich API, the author of a library can supplement
 the default types of the library's API with typing rules that improve the
 collaboration between host programs and uses of the library. The
 evaluation uses a prototype for Typed Racket and illustrates how useful
 the idea is for widely available libraries. Also the paper sketches how
 the authors of such ``tailored'' rules can argue their soundness.

}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@; See OUTLINE.md for explanation
@include-section{intro.scrbl}
@include-section{background.scrbl}
@include-section{segfault.scrbl}
@;@include-section{examples.scrbl}
@;@include-section{discussion.scrbl}
@;@include-section{friends.scrbl}
@;@include-section{related-work.scrbl}
@;@include-section{conclusion.scrbl}

@section[#:style 'unnumbered]{Acknowledgments}

To appear

@;We thank
@;Sam Tobin-Hochstadt for reminding us that Typed Racket is macro-extensible,
@;Ryan Culpepper for divulging secrets of the Racket macro system,
@;Asumu Takikawa and Leif Andersen for rejecting some earlier designs,
@;Justin R. Slepak for teaching us the term ``textualist''.
@;ICFP reviewers for thoughtful comments
@;and Northeastern PLT for comments on an earlier draft.

@generate-bibliography[#:sec-title "References"]
