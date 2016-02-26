#lang scribble/manual
@;#lang scribble/sigplan @onecolumn 

@(require "common.rkt")

@authorinfo["Ben Greenman and Matthias Felleisen"
            "Northeastern University, Boston, USA"
            ""]

@title{Functional Pearl: Do we @emph{really} need Dependent Types?}
@; Trivial differences, little things that count
@; Recall 'fexprs are trivial'?
@; Syntactically dependent types

@abstract{
  The transformative @emph{and} analytic power of macros can turn
   a polymorphic type system into a dependent type system, at least
   for common programming tasks.
  By analyzing program syntax and propogating known information about program
   @emph{values} at compile-time, we can express many of the practical
   motivations for dependent types without requiring programmer annotations
   or changes to the underlying type system.

  Our macro-expanded types are not proving new theorems,
   but they recognize facts obvious to the programmer and hopefully
   give a nice programming experience.

}

@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}
@;@keywords{Gradual typing, performance evaluation}

@include-section{outline.scrbl}

@;@section[#:style 'unnumbered]{Acknowledgments}

@generate-bibliography[#:sec-title "References"]
