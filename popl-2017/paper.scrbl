#lang scribble/sigplan @onecolumn @preprint

@(require "common.rkt")

@title{Tailoring Type Theories (T.T.T)}
@authorinfo["Piet Hein" "Dutchman" "piet at hein.com"]

@abstract{

A static type system is a compromise between precision and usability.
 Improving the ability of a type system to distinguish correct and
 erroneous programs typically requires that programmers restructure their
 code or provide more type annotations, neither of which are desirable
 tasks.

This paper presents an elaboration-based technique for refining the
 analysis of an existing type system on existing code without changing the
 type system or the code.  As a proof of concept, we have implemented the
 technique as a Typed Racket library.  From the programmers' viewpoint,
 simply importing the library makes the type system more perceptive---no
 annotations or new syntax are required.  

}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@; See OUTLINE.md for explanation
@include-section{intro.scrbl}
@include-section{background.scrbl}
@include-section{examples.scrbl}
@include-section{discussion.scrbl}
@include-section{friends.scrbl}
@include-section{related-work.scrbl}
@include-section{conclusion.scrbl}

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
