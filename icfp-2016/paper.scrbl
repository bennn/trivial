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
  A static type system is a compromise between rejecting all bad programs
   and approving all good programs, where @emph{bad} and @emph{good} are
   formalized in terms of a language's untyped operational semantics.
  Consequently, every useful type system rejects some well-behaved programs
   and approves other programs that go wrong at runtime.
  Improving the precision of a language's type system is difficult for
   everyone involved---designers, implementors, and users.

  This pearl presents a simple, elaboration-based technique for refining the
   analysis of an existing type system; that is,
   we approve more good programs and reject more bad ones.
  The technique may be implemented as a library and requires no annotations
   from the programmer.
  A straightforward (yet immoral) extension of the technique is shown to
   improve the performance of numeric and vector operations.
}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@include-section{intro.scrbl}
@include-section{solution.scrbl}
@include-section{usage.scrbl}
@include-section{experience.scrbl} @; Merge with usage?
@include-section{implementation.scrbl}
@;@include-section{correctness.scrbl}
@;@include-section{related.scrbl}
@;@include-section{conclusion.scrbl}

@;@section[#:style 'unnumbered]{Acknowledgments}
@;
@;We thank
@;Sam Tobin-Hochstadt for reminding us that Typed Racket is macro-extensible,
@;Ryan Culpepper for divulging secrets of the Racket macro system,
@;Asumu Takikawa and Leif Andersen for rejecting some earlier designs,
@;Matthias Felleisen for sharing his worldview,
@;and Northeastern PLT for comments on an earlier draft.

@generate-bibliography[#:sec-title "References"]
