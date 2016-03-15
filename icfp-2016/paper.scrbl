#lang scribble/sigplan @onecolumn @preprint

@(require "common.rkt")

@authorinfo["???" "???" ""]
@;@authorinfo["Ben Greenman and Stephen Chang"
@;            "PLT @ Northeastern University, Boston, USA"
@;            ""]

@title{Functional Pearl: Do you see what I see?}
@; @subtitle{Improving a simple type system with dependent macros}
@; @subtitle{Value-based partial evaluation}
@; @subtitle{Read-time constant folding}
@; TODO subtitle doesn't appear in the right place

@abstract{
  A static type system is a compromise between precision and usability.
  Improving the ability of a type system to distinguish correct and erroneous
   programs typically requires that programmers restructure their code or
   provide more type annotations, neither of which are desirable tasks.

  This pearl presents an elaboration-based technique for refining the
   analysis of an existing type system on existing code
   @emph{from outside} the type system.
  We have implemented the technique as a Typed Racket library.
  From the programmers' perspective, simply importing the library makes the type
   system more perceptive---no annotations or new syntax required.
}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@include-section{intro.scrbl}
@include-section{solution.scrbl}
@include-section{usage.scrbl}
@include-section{experience.scrbl} @; Merge with usage?
@include-section{implementation.scrbl}
@include-section{conclusion.scrbl}

@;@section[#:style 'unnumbered]{Acknowledgments}
@;
@;We thank
@;Sam Tobin-Hochstadt for reminding us that Typed Racket is macro-extensible,
@;Ryan Culpepper for divulging secrets of the Racket macro system,
@;Asumu Takikawa and Leif Andersen for rejecting some earlier designs,
@;Matthias Felleisen for sharing his worldview,
@;and Northeastern PLT for comments on an earlier draft.

@generate-bibliography[#:sec-title "References"]
