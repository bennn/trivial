#lang scribble/manual

@title{Do You See What I See, in review}

Rejected at ICFP, making ready for a POPL submission.


@section{SoundX}
@subsection{What is SoundX?}

@hyperlink["http://conf.researchr.org/event/POPL-2016/popl-2016-papers-sound-type-dependent-syntactic-language-extension"]{SoundX} is a system for:
@itemlist[
  @item{
    @emph{Modeling} programming languages.
  }
  @item{
    Writing type-sound extensions to a base language.
    The extensions are new type derivations expressed in terms of the base language's type system.
  }
]

@; TODO possible to run soundx programs?

@subsection{Key Features of SoundX}
@itemlist[
  @item{
    Proposes a new idea: desugarings act on type derivations are are guaranteed
    to produce only well-typed terms.
  }
  @item{
    Detailed metatheory (may be useful reference for others)
  }
  @item{
    Models a subset of Java and implements Scala-inspired extensions.
    (Everyone loves Java and nobody @emph{really} trusts Scala.)
  }
]

@subsection{Lessons from SoundX}
@itemlist[
  @item{
    Errors must be reported in terms of the source language, not the desugared result
  }
  @item{
    Type errors in code produced by a desugaring must reflect USER type errors,
    not errors in the desugaring rules.
  }
  @item{
    Desugarings should have access to type information.
  }
]


@section{Our Contribution}


