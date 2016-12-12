#lang scribble/manual
@require[
  (for-label
    racket/base)
]

@; =============================================================================

@title[#:tag "ttt:api"]{Defining new Tailoring}

@defmodule[trivial/tailoring]

The bindings documented in this section are not provided by the @racketmodname[trivial] module.

Defining a new tailoring is not easy, but you can technically do it without making a pull request to this library.
There are three steps to making a tailoring:
@itemlist[
@item{
  identify a property of Racket expressions,
}
@item{
  define when / how to infer this property from @racket[#%datum] literals,
}
@item{
  define a set of tailorings that infer, propagate, and use the property
}
]

See the @racketmodname[trivial/tailoring] module for a list of exported identifiers, and the source code for example uses.
Sorry for not documenting further, but the API is subject to change.

@bold{Note:} at the moment, the properties tracked by other modules (e.g. @racketmodname[trivial/regexp]) are not public.


@;@section{Further Reading}
@;
@;Short essay: @hyperlink["http://www.ccs.neu.edu/home/types/publications/letter-of-values/macro-goggles.pdf"]{here}.
@;
@;
@;@section{Credits}
@;
@;Inspired by:
@;@itemlist[
@;  @item{@hyperlink["http://www.ccs.neu.edu/home/dherman/research/papers/icfp04-dsl-analysis.pdf"]{David Herman and Philippe Meunier}}
@;  @item{@hyperlink["http://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen}}
@;  @item{@hyperlink["http://www.ccs.neu.edu/home/stchang/"]{Stephen Chang}}
@;  @item{@hyperlink["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt}}
@;]
@;
