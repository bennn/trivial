#lang scribble/manual
@require[
  scribble/example
  (for-template trivial/tailoring)
  (for-label
    racket/base
    racket/contract
    syntax/parse
    trivial/tailoring
    (only-in typed/racket/base Integer String Exact-Number Char))
]

@(define (make-trivial-eval) (make-base-eval #:lang 'racket/base '(begin (require trivial))))
@(define integer-domain-eval (make-base-eval #:lang 'racket/base '(begin (require (for-template trivial/tailoring) syntax/parse))))

@; =============================================================================

@title[#:tag "ttt:api"]{Defining new Tailorings}

@defmodule[trivial/tailoring]

The bindings documented in this section are not provided by the @racketmodname[trivial] module.

Defining a new tailoring is not easy, but you can technically do it without making a pull request to this library.
There are three steps to making a tailoring:
@itemlist[
@item{
  identify a property of Racket expressions,
}
@item{
  define when & how to infer this property from @racket[#%datum] literals,
}
@item{
  define a set of tailorings that infer, propagate, and use the property
}
]

For example, here is a tailoring for @racket[add1].
If the value of its argument is known during expansion, it expands to an integer.

@codeblock{
  #lang racket/base
  (require
    trivial/tailoring
    (for-syntax racket/base)
    (only-in racket/base [add1 λ-add1])
    (only-in typed/racket/base [add1 τ-add1]))

  (define-tailoring (-add1 [e ~> e+ (φ [integer-domain ↦ i])])
    #:with +add1 (τλ #'τ-add1 #'λ-add1)
    #:= (⊥? integer-domain i)
        (+add1 e+)
    #:+ #t
        '#,(+ i 1)
    #:φ (φ-set (φ-init) integer-domain (reduce integer-domain + i 1)))
}

Here is how to read the un-hygienic @racket[define-tailoring] form:
@itemlist[
@item{
  @racket[-add1] is a macro expecting one argument, named @racket[e].
  (If @racket[-add1] is called with zero or +2 arguments, it expands to Racket's @racket[add1].)
}
@item{
  @margin-note{Note: the key @racket[integer-domain] describes the relationship between @racket[e] and @racket[i]. In short, if tailorings that interact with @racket[integer-domain] are implemented correctly, then expanding @racket[e] yields and expression @racket[e+] with value @racket[i] associated with the key @racket[integer-domain] @emph{if and only if} all run-time values for @racket[e] are integers with value @racket[i].}
  @racket[-add1] expands its argument @racket[e] to the term @racket[e+] and retrieves a dictionary @racket[φ] of @emph{tailoring properties} (see @secref{ttt:properties})  associated with @racket[e+].
  It furthermore pattern-matches the dictionary @racket[φ] to get the integer value @racket[i] of the expression @racket[e+].
}
@item{
  @racket[-add1] conditionally produces one of two syntax objects.
  @itemlist[
    @item{
      In the neutral case (@racket[#:=]), when @racket[i] is an indeterminate value, the result is a call to Racket's (or Typed Racket's) @racket[add1].
    }
    @item{
      In the positive case (@racket[#:+]), when @racket[i] is known during expansion, the result is the integer @racket[(+ i 1)]
    }
  ]
}
@item{
  Any syntax object produced by expanding a call to @racket[-add1] with one argument has a new dictionary of tailoring properties (@racket[#:φ]).
  The dictionary has one key, @racket[integer-domain].
  The corresponding value is "the indeterminate integer" when the value of @racket[i] is indeterminate and @racket[(+ i 1)] otherwise.
}
]

Here is the general form of @racket[define-tailoring]:

@defform[(define-tailoring (tailored-id arg-pattern ...)
           guard ...
           defn ...
           #:= test-expr template
           #:+ then-expr template
           #:- then-expr expr
           #:φ prop-expr)
         #:grammar
         ([arg-pattern (e-id elab-> e+-id (φ-id [key-id map-> val-id] ...))]
          [guard (code:line #:with expr expr)
                 (code:line #:when expr)
                 (code:line #:fail-unless expr expr)]
          [defn (define . expr)]
          [test-expr expr]
          [prop-expr expr]
          [elab-> ~> ⇝]
          [map-> -> ↦])]{

  Defines a tailoring @racket[tailored-id].

  The @racket[arg-pattern] clauses describe the shape of arguments to @racket[tailored-id].
  These say: expand @racket[e-id] to @racket[e+-id], retrieve the tailoring properties @racket[φ-id] associated with @racket[e+-id], and finally retrieve the values @racket[val-id] associated with the given @racket[key-id].

  If an @racket[arg-pattern] is followed by a literal @racket[...], then the tailoring accepts any number of arguments and matches each against @racket[arg-pattern].
  See the example for @racket[map] below.

  The @racket[guard] expressions are a subset of the guards recognized by @racket[syntax-parse].
  Guards are most useful for defining variables based on the context in which the macro is expanding; see for example the @racket[#:with] guard in @racket[-add1].

  The @racket[defn] expressions introduce local bindings.

  @margin-note{The reason for @racket[#:+] etc. is so that @racket[define-tailoring] can (1) log @racket['info] messages for successful (@racket[#:+]) and unsuccessful (@racket[#:=]) tailorings (2) wrap each @racket[template] in a @racket[syntax/loc] based on use-sites of the tailoring.}
  The @racket[#:=], @racket[#:+], and @racket[#:-] clauses are a @racket[cond]-like domain-specific language.
  Their order matters.
  The @racket[test-expr] are typically expressions involving the @racket[val-id] bound above.
  In general, the @racket[template] forms describe how the tailoring produces code (see @racket[syntax] for their grammar).
  But the specific meaning of each @racket[template] should correspond to its keyword:
  @itemlist[
  @item{
    @racket[#:= test-expr template] uses @racket[test-expr] to check whether the tailoring @bold{does not} have enough information to transform the program.
    The @racket[template] should implement "the standard Racket behavior" for @racket[tailored-id].
  }
  @item{
    @racket[#:+ test-expr template] determines when the tailoring can transform the program.
    The @racket[template] may use unsafe operations, additional type annotations, or partial evaluation.
  }
  @item{
    @racket[#:- test-expr expr] detects errors statically and invokes @racket[expr] to raise an exception.
    This clause is optional; when omitted, raises an internal @racket[exn:fail?] about the inexhaustive tailoring.
  }
  ]

  Finally, @racket[prop-expr] describes the result with a new dictionary of tailoring properties.

  @bold{Note} if any @racket[val-id] is bound to a "top element" of the domain @racket[key-id], then @racket[define-tailoring] raises an error message.
  See @secref{ttt:domains} for further intuition.

  @margin-note{These examples contain free variables, but should convey the main ideas of @racket[define-tailoring]. See the library implementation for working versions.}
  Example: a @racket[vector-length] that computes the length statically.
  @codeblock{
    (define-tailoring (-vector-length [e ~> e+ (φ [vector-domain v])])
      #:with +vl (τλ #'τ-vector-length #'λ-vector-length)
      (define n (vector-domain-length v))
      #:= (⊥? V-dom v)
          (+vl e+)
      #:+ #t '#,n
      #:φ (φ-set (φ-init) integer-domain n))
  }

  Example: a @racket[vector-ref] that checks for out-of-bounds references and replaces in-bounds references with unsafe accesses.
  @codeblock{
    (define-tailoring (-vector-ref [e1 ~> e1+ (φ1 [vector-domain ↦ v])]
                                   [e2 ~> e2+ (φ2 [integer-domain ↦ i])])
       #:with +vector-ref (τλ #'τ-vector-ref #'λ-vector-ref)
       #:with +unsafe-vector-ref (τλ #'τ-unsafe-vector-ref #'λ-unsafe-vector-ref)
       (define n (vector-domain-length v))
       #:= (or (⊥? vector-domain v) (⊥? integer-domain i))
           (+vector-ref e1+ e2+)
       #:+ (and (<= 0 i) (< i n))
           (+unsafe-vector-ref e1+ e2+)
       #:- #t
           (format-bounds-error #'e1+ i)
       #:φ (vector-domain-ref v i))
  }

  Example: a @racket[map] that statically checks arity and propagates the length of the result.
  @codeblock{
    (define-tailoring (-map [f ~> f+ (φ1 [A-dom a])]
                            [e* ~> e+* (φ* [L-dom l*])] ...)
      #:with +map (τλ #'τ-map #'λ-map)
      (define expected-arity (length l*))
      (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
      (define n* (map L->I l*))
      #:= (and (⊥? I-dom (⊓* I-dom n*)) arity-ok?)
          (+map f+ e+* ...)
      #:+ arity-ok?
          (+map f+ e+* ...)
      #:- #t
          (format-arity-error #'f+ expected-arity)
      #:φ (φ-set (φ-init) L-dom (I->L (⊓* I-dom n*))))
  }
}


@section[#:tag "ttt:domains"]{Property Domains}

Every tailoring extracts and asserts properties about program expressions.
For example, tailorings in @racketmodname[trivial/vector] assert properties about the size and contents of vector values.
Tailorings in @racketmodname[trivial/format] assert properties about the formatting escapes in string values.
The "properties of vectors" and "properties of format strings" are two examples of @emph{property domains}.

A property domain is a lattice.
In particular, each property domain @racket[D] has:
@itemlist[
@item{
  A distinguished bottom element @racket[(⊥ D)].

  Interpretation: unknown property.
}
@item{
  A (family of) top element(s) @racket[(⊤ D message)].

  Interpretation: contradictory property, because @racket[message].
}
@item{
  A family of elements.
}
@item{
  A partial order @racket[<=?] relating elements to one another, and to the top and bottom element.
  For any element @racket[e] and any string @racket[message], both @racket[(<=? (⊥ D) e)] and @racket[(<=? e (⊤ D message))] hold.
}
]

@defform[(make-property-domain sym #:leq order clause ...)
         #:contracts ([sym symbol?]
                      [order (-> any/c any/c any/c)])]{
  Builds a new property domain from a symbol @racket[id], ordering relation @racket[order], and list of @racket[syntax-parse] clauses.
  Specifically:
  @itemlist[
  @item{
    generates symbols to represent the top and bottom elements,
  }
  @item{
    lifts @racket[order] to account for these top and bottom elements,
  }
  @item{
    creates a @racket[syntax-parser] from @racket[clause ...],
  }
  @item{
    registers the new syntax parser with @racketmodname[trivial/define].
  }
  ]

  The default @racket[order] does not relate any elements to one another.
  It is @racket[(λ (x y) #false)].

  @examples[#:eval integer-domain-eval
    (define integer-domain
      (make-property-domain Z #:leq <=
       [i:integer (syntax-e #'i)]))

    (define flat-integer-domain
      (make-property-domain flat-Z
       [i:integer (syntax-e #'i)]))
  ]
}

@defproc[(property-domain? [v any/c]) boolean?]{
  Predicate for values returned by @racket[make-property-domain].
}

@defproc[(in-domain? [D property-domain?]) (-> any/c boolean?)]{
  Predicate for values in a domain.
}

@defproc[(⊥ [D property-domain?]) (in-domain? D)]{
  Return the bottom element of @racket[D].
}

@defproc[(⊤ [D property-domain?] [str string?]) (in-domain? D)]{
  Return a top element of @racket[D], with the error message @racket[str].
  If a tailoring encounters this element, it uses @racket[str] to explain an error to the programmer.

  @examples[#:eval (make-trivial-eval)
    (define bad-rx "foo ( bar")
    (eval:error (regexp-match bad-rx "any string"))
  ]
}

@deftogether[(
  @defproc[(⊥? [D property-domain?] [v any/c]) boolean?]{}
  @defproc[(⊤? [D property-domain?] [v any/c]) boolean?]{}
)]{
  Predicates for the bottom and top elements of the given domain.
}

@deftogether[(
  @defproc[(glb [D property-domain?] [v (in-domain? D)] ...) (in-domain? D)]{}
  @defproc[(⊓ [D property-domain?] [v (in-domain? D)] ...) (in-domain? D)]{}
  @defproc[(glb* [D property-domain?] [lst (listof (in-domain? D))]) (in-domain? D)]{}
  @defproc[(⊓* [D property-domain?] [lst (listof (in-domain? D))]) (in-domain? D)]{}
)]{
  Greatest-lower-bound on the domain @racket[D].
  Assumes that none of its arguments are top elements.

  @examples[#:eval integer-domain-eval
    (glb integer-domain 1 2 -3 0)
    (⊥? integer-domain (glb integer-domain 1 2 (⊥ integer-domain)))
  ]
}

@deftogether[(
  @defproc[(lub [D property-domain?] [v (in-domain? D)] ...) (in-domain? D)]{}
  @defproc[(⊔ [D property-domain?] [v (in-domain? D)] ...) (in-domain? D)]{}
  @defproc[(lub* [D property-domain?] [lst (listof (in-domain? D))]) (in-domain? D)]{}
  @defproc[(⊔* [D property-domain?] [lst (listof (in-domain? D))]) (in-domain? D)]{}
)]{
  Least-upper-bound on @racket[D].
  Assumes that none of its arguments are top elements.

  @examples[#:eval integer-domain-eval
    (lub* integer-domain '(4 5 6))
    (lub integer-domain 1 2 (⊥ integer-domain))
  ]
}

@deftogether[(
  @defproc[(reduce [D property-domain?] [f (-> any/c any/c any/c)] [init any/c] [v (in-domain? D)] ...) (in-domain? D)]{}
  @defproc[(reduce* [D property-domain?] [f (-> any/c any/c any/c)] [init any/c] [lst (listof (in-domain? D))]) (in-domain? D)]{}
)]{
  Combine the elements @racket[v ...] (or list of elements @racket[lst]) using the binary function @racket[f].
  @racket[reduce] lifts the given function @racket[f] to handle top and bottom elements in @racket[D].

  @examples[#:eval integer-domain-eval
    (reduce integer-domain + 8 6 7 5 3 0 9)
    (reduce flat-integer-domain + 0 2 2)
  ]
}


@subsection{Built-in Property Domains}

The following property domains are provided by the @racketmodname[trivial/tailoring] module.

@defthing[arity-domain property-domain?]{
  Flat lattice of formal parameters to a function.

  For example, the arity of:
  @racketblock[
    (λ (x y z) x)
  ]
  is:
  @racketblock[
    (x y z)
  ]

  Similarly, the arity of:
  @racketblock[
    (λ ([a : String] [b : Integer]) 42)
  ]
  is:
  @racketblock[
    ([a : String] [b : Integer])
  ]
}
@defthing[format-string-domain property-domain?]{
  Flat lattice of sequences of Typed Racket types; in particular, an element of @racket[format-string-domain] satisfies the contract:
  @racketblock[
    (listof (or/c Char Exact-Number))
  ]
  These represent the values that a format string expects, given its formatting escapes.
}

@defthing[integer-domain property-domain?]{
  Totally-ordered lattice of integers, using @racket[<=].
}


@defthing[list-domain property-domain?]{
  Pointwise-ordered lattice of lists of tailoring properties.
}

@defthing[regexp-group-domain property-domain?]{
  Flat lattice of lists of booleans.
  The boolean value at position @racket[i] determines whether the @racket[i]-th group in the regular expression is guaranteed to capture a string.
  For example, the domain element:
  @racketblock[
    (list #true #false)
  ]
  describes regular expressions that capture two groups; the second group may fail to capture.
  One regular expression that is described by this domain element is:
  @racketblock[
    #rx"(a*)(b)*"
  ]
}

@defthing[vector-domain property-domain?]{
  Pointwise-ordered lattice of lists of tailoring properties.
}

The @racket[list-domain] and @racket[vector-domain] have additional operations.
These are @racket[⊥]-propagating versions of their @racketmodname[racket/list] and @racketmodname[racket/vector] equivalents.
(And there is probably a better or more-general way to lift these computations.)

@deftogether[(
  @defproc[(vector-domain->list-domain [v (in-domain? vector-domain)]) (in-domain? list-domain)]{}
  @defproc[(list-domain->vector-domain [l (in-domain? list-domain)]) (in-domain? vector-domain)]{}
)]{
  Defines an isomorphism between the list and vector domains.
}

@deftogether[(
  @defproc[(list-domain->integer-domain [l (in-domain? list-domain)]) (in-domain? integer-domain)]{}
  @defproc[(vector-domain->integer-domain [v (in-domain? vector-domain)]) (in-domain? integer-domain)]{}
)]{
  "Forgetful functors" that abstract a sequence with its length.
}

@deftogether[(
  @defproc[(integer-domain->list-domain [i (in-domain? integer-domain)]) (in-domain? list-domain)]{}
  @defproc[(integer-domain->vector-domain [i (in-domain? integer-domain)]) (in-domain? vector-domain)]{}
)]{
  "Right adjoints" to the above; these convert an integer to a sequence of empty tailoring property maps.
}

@deftogether[(
  @defproc[(list-domain-append* [lst* (listof (in-domain? list-domain))]) (in-domain? list-domain)]{}
  @defproc[(list-domain-cons [prop φ?] [lst (in-domain? list-domain)]) (in-domain? list-domain)]{}
  @defproc[(list-domain-car [lst (in-domain? list-domain)]) φ?]{}
  @defproc[(list-domain-cdr [lst (in-domain? list-domain)]) (in-domain? list-domain)]{}
  @defproc[(list-domain-length [lst (in-domain? list-domain)]) (in-domain? integer-domain)]{}
  @defproc[(list-domain-ref [lst (in-domain? list-domain)] [pos (in-domain? integer-domain)]) φ?]{}
  @defproc[(list-domain-reverse [lst (in-domain? list-domain)]) (in-domain? list-domain)]{}
  @defproc[(list-domain-set [lst (in-domain? list-domain)] [pos (in-domain? integer-domain)] [prop φ?]) (in-domain? list-domain)]{}
  @defproc[(list-domain-slice [lst (in-domain? list-domain)] [lo (in-domain? integer-domain)] [hi (in-domain? integer-domain)]) (in-domain? list-domain)]{}
)]{
  Extra functions on list domain elements.
}

@deftogether[(
  @defproc[(vector-domain-append* [lst* (vectorof (in-domain? vector-domain))]) (in-domain? vector-domain)]{}
  @defproc[(vector-domain-length [lst (in-domain? vector-domain)]) (in-domain? integer-domain)]{}
  @defproc[(vector-domain-ref [lst (in-domain? vector-domain)] [pos (in-domain? integer-domain)]) φ?]{}
  @defproc[(vector-domain-set [lst (in-domain? vector-domain)] [pos (in-domain? integer-domain)] [prop φ?]) (in-domain? vector-domain)]{}
  @defproc[(vector-domain-slice [lst (in-domain? vector-domain)] [lo (in-domain? integer-domain)] [hi (in-domain? integer-domain)]) (in-domain? vector-domain)]{}
)]{
  Extra functions on vector domain elements.
}


@section[#:tag "ttt:properties"]{Tailoring Properties}

Tailorings associate static properties to expressions using finite maps @racket[φ].
Each map relates property domains to values in the domain (or the bottom element of the domain).

@defproc[(φ [stx syntax?]) φ?]{
  Get the tailoring properties associated with the syntax object @racket[stx].
}

@defproc[(φ? [v any/c]) boolean?]{
  Predicate for tailoring property maps.
}

@defproc[(φ-init) φ?]{
  Return an empty map.
}

@defproc[(φ-ref [prop φ?] [D property-domain?]) (in-domain? D)]{
  Return the value associated with domain @racket[D] in the map @racket[prop].
}

@defproc[(φ-set [prop φ?] [D property-domain?] [v (in-domain? d)]) φ?]{
  Return a map like @racket[prop], but with @racket[v] associated with the domain @racket[D].
}

@defproc[(φ<=? [p1 φ?] [p2 φ?]) boolean?]{
  Pointwise ordering on tailoring property maps.
  Returns true when @racket[p2] has bindings for every domain @racket[D] bound in @racket[p1], and furthermore the value @racket[v1] associated with @racket[D] in @racket[p1] is less than the value @racket[v2] associated with @racket[D] in @racket[p2] (using the order from @racket[D]).
}


@section{Miscellaneous}

@defform[(τλ typed-id untyped-id)]{
  Chooses @racket[typed-id] or @racket[untyped-id] depending on whether the
   currently-expanding module is from @racketmodname[typed/racket] or not.
}

@defthing[trivial-logger logger?]{
  Logs events to the @racket{ttt} topic.
}

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
