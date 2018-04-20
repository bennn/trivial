#lang scribble/manual
@require[
  racket/include
  scribble/eval
  scriblib/footnote
  (for-label
    racket/base
    (only-in typed/racket/base String))
]

@(define (reftech . word*)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") word*))

@; =============================================================================

@title[#:tag "top"]{Trivial: type-tailored library functions}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[trivial]
@(define trivial-eval (make-base-eval #:lang 'typed/racket/base '(begin (require trivial))))

The @racketmodname[trivial] module exports @emph{tailored} versions of standard library functions.

@section[#:tag "ttt:introduction"]{What is Tailoring?}
A tailored function @racket[f+]:
@itemlist[
@item{
  has the same specification and behavior as some library function @racket[f]
}
@item{
  but can catch runtime errors during @reftech{expansion}
}
@item{
  and may typecheck a little smoother in Typed Racket.
}
]

@subsection{Examples of Tailoring}
For example, make a new Racket module with a malformed call to @racket[format]:

@codeblock{
  #lang racket/base

  (format "batman and ~a")
}

This file will compile no problem (@tt{raco make file.rkt}), but will raise a runtime error (@tt{racket file.rkt}).
If you add the line @racket[(require trivial)], the file no longer compiles, but instead gives a helpful error message.


Here's another example.
Save the following (correct) Racket module:

@codeblock{
  #lang racket/base

  (define (get-sidekick str)
    (cond
     [(regexp-match #rx"^([a-z]+) and ([a-z]+)$" str)
      => caddr]
     [else "???"]))

  (get-sidekick "batman and alfred")
}

It should compile and run, returning @racket{alfred}. Great.
Now try converting it to Typed Racket:

@codeblock{
  #lang typed/racket/base

  (define (get-sidekick (str : String)) : String
    (cond
     [(regexp-match #rx"^([a-z]+) and ([a-z]+)$" str)
      => caddr]
     [else "???"]))

  (get-sidekick "batman and alfred")
}

The file doesn't compile anymore.
Adding @racket[(require trivial)] to the top of the file removes the error.


@subsection{Technical Description}

A tailored function @racket[f+] is really a macro that examines its call site and attempts to combine knowledge about the behavior of @racket[f] with @emph{static properties} of its arguments.
If all goes well, the tailoring will either (1) identify an error or (2) transform the call site into an equivalent---but more efficient or more Typed-Racket-friendly---call.
Otherwise, the call to @racket[f+] behaves exactly as a call to @racket[f] would.

In general, the @emph{static properties} could be the result of any static analysis.
But this library is limited to properties that other macros can establish through local analysis and propagate via @reftech{syntax properties}.
See @secref{ttt:api} for more details.

@include-section{using-tailorings.scrbl}
@include-section{defining-tailorings.scrbl}

