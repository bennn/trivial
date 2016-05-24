#lang scribble/sigplan
@require["common.rkt"]


@title[#:tag "sec:rw"]{Related Work}

SoundX is a system for modeling programming languages and defining type-sound
 extensions, e.g. defining a type derivation for @tt{let} in terms of a type
 derivation for @tt{λ} expressions.
However, it is not possible to define new binding forms like @tt{letrec} if
 they cannot be expressed in terms of a core language binding form.
All language extensions are desugared @emph{after} the source program is type-checked
 and all desugarings are @emph{guaranteed} to produce only well-typed terms.
For an extended example, the authors model a subset of Java and extend the model
 with Scala-style for comprehensions.
Type correctness is a relatively shallow notion of correctness for a language
 extension, but still their ideas are pretty cool.

We propose a deeper notion of correctness for our syntactic transformations, but
 keep in mind SoundX's criteria:
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
Though we fail on the third point.
