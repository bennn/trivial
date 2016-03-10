#lang scribble/sigplan @onecolumn @preprint

@(require "common.rkt")

@authorinfo["Ben Greenman and Stephen Chang"
            "PLT @ Northeastern University, Boston, USA"
            ""]

@title{Functional Pearl: Do you see what I see?}
@; @subtitle{Improving a simple type system with dependent macros}
@; should say "value-dependent"?
@; TODO subtitle doesn't appear in the right place

@abstract{
  TBA @; Be Positive, not negative
  @;A simple type system with macros is nearly as good as a dependent type
  @; system, at least for some common programming tasks.
  @;By analyzing program syntax and propogating constants
  @; before type-checking, we can express many of the practical
  @; motivations for dependent types without any programmer annotations
  @; or extensions to the underlying type system.

  @;Our syntax-dependent subtypes are not proving theorems,
  @; but they detect certain run-time errors at compile-time and
  @; cooperate with type inference.
}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@include-section{intro.scrbl}
@;@include-section{solution.scrbl}
@;@include-section{usage.scrbl}
@;@include-section{experience.scrbl} @; Merge with usage?
@;@include-section{implementation.scrbl}
@;@include-section{correctness.scrbl}
@;@include-section{related.scrbl}
@;@include-section{conclusion.scrbl}

@section[#:style 'unnumbered]{Acknowledgments}

Sam Tobin-Hochstadt for reminding us that Typed Racket is macro-extensible,
Stephen Chang for teaching us syntax properties and rename transformers,
Ryan Culpepper for macrology advice,
Asumu Takikawa and Leif Andersen for helpful discussions,
Jack Firth for using the library,

@generate-bibliography[#:sec-title "References"]
