#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:implementation"]{Implementation}
@; Amazing Macros
@; Whirlwind tour
@; Accounting, taking stock
@; Alas I knew him well


@; TODO this is all so boring right now, need to revise heavily

@Figure-ref{fig:stats} gives a few statistics regarding our implementation.
The purpose of this section is to explain why the numbers are low.
@; Ode to macros, implementation a symphony

@figure["fig:stats"
  "Quantifying the implementation"
  @exact|{\input{fig-stats}}|
]

In total, our six applications comprise 800 lines of code (LOC).
Another 145 lines implement common functionality, putting the grand total
 just under 1000 LOC.

Except for @exact|{\mod{db}}| and @exact|{\mod{regexp}}|, each of the
 core modules defines a single function in @exact|{$\interp$}|.
In @exact|{\mod{db}}| the two functions are the schema predicate and @tt{SQL}
 query parser (we omit the trivial interpreter for connections).
    @; TODO no parentheses?
On the other hand, @exact|{\mod{regexp}}| implements six group-parsing functions
 to match the six string-like input types
   @;@note{Strings, Regex literals, Posix Regex literls, and byte-string variations of each.}
 accepted by Racket's @racket[regexp-match].
These group parsers, however, share a 33-line kernel.
Incidentally, the average size of all value-interpreting functions is 33 LOC.
The smallest interpreter is the composition of Racket's @racket[number?] predicate
 with the identity interpretation in @exact|{\mod{math}}| (3 LOC).
The largest is the query parser (35 LOC), though the analyses for
 format strings and regular expressions are approximately the same size.

The @exact{$\trans$} functions are aliases for standard library procedures.
   @; TODO much better to show off short code. But let's draft it first.
In many cases we are able to re-use code between similar functions.
For instance, the arithmetic operators @racket[+ - * /] are implemented by
 a single fold.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:impl-trans"]{Implementing Transformations} @; TODO not a great name
@; @section{Ode to Macros: The Long Version}

At this point, we have carried on long enough talking about the implementation
 without actually showing any code.
No longer---here is our definition of @racket[vector-length]:

@codeblock{
  (make-alias #'vector-length
    (syntax-parser
     [(_ v:vector/length)
      #''v.evidence]
     [_ #false]))
}

First of all, this transformation works as specified in @Secref{sec:vector}.
When the length of its argument is known, it expands to that length.
Otherwise, it expands to an ordinary call to @racket[vector-length].

Second, we need to introduce a few mysterious characters:
@itemlist[
  @item{
    @racket[(make-alias id f)] creates a transformation from an identifier @racket[id]
    and a partial function @racket[f].
  }
  @item{
    The symbol @tt{#'} creates a syntax object from a value or template.
  }
  @item{
    A @racket[syntax-parser] is a match statement over syntactic patterns.
    This parser recognizes two cases: application to a single argument via
     the pattern @racket[(_ v:vector/length)] and anything else with the
     wildcard @tt{_}.
  }
  @item{
    The colon character (@tt{:}) used int @racket[v:vector/length]
     binds the variable @racket[v] to the @emph{syntax class} @racket[vector/length].
  }
  @item{
    The dot character (@tt{.}) accesses an @emph{attribute} of the value bound
     to @racket[v].
    In this case, the attribute @racket[evidence] is set when 
     @racket[vector/length] matches successfully.
  }
]

Third, we remark that the pattern @racket[v:vector/length] unfolds all
 transformations to @racket[v] recursively.
So we handle each of the following cases, as well as any other combination of
 length-preserving vector operations.

@racketblock[
> '(vector-length #(H I))
2
> '(vector-length (vector-append #(Y O)
                                 #(L O)))
4
]

@; The general features are explained in greater deteail below

@; make-alias
@; TODO variable name for f
The overall structure of @racket[vector-length] is common to many of our transformations.
That is, we define a rule to handle an interesting syntactic pattern and
 then generate an alias from the rule using the helper function @racket[make-alias].

@codeblock{
  (define ((make-alias orig-id f) stx)
    (or (f stx)
        (syntax-parse stx
         [_:id
          orig-id]
         [(_ e* ...)
          #`(#,orig-id e* ...)])))
}

The transformation defined by @racket[(make-alias id f)] is a function on
 syntax objects.
First, the function applies @racket[f] to the syntax object @racket[stx].
If the result is not
 @racket[#false] we return.
Otherwise the function matches its argument against two possible patterns:
@itemize[
  @item{
    @tt{_:id} recognizes identifiers with the built-in syntax class @racket[id].
    When this pattern succeeds, we return the aliased @racket[orig-id].
  }
  @item{
    @racket[(_ e* ...)] matches function application.
    In the result of this branch,
     @; TODO backtick not printing right
     we declare a syntax template with @tt{#`} and splice the identifier
     @racket[orig-id] into the template with @tt{#,}.
    These operators are formally known as @racket[quasisyntax] and @racket[unsyntax];
     you may know their cousins @racket[quasiquote] and @racket[unquote].
  }
]

@emph{Note:} the identifier @racket[...] is not pseudocode!
In a pattern, it captures zero-or-more repetitions of the preceding pattern---in
 this case, the variable @racket[e*] binds anything so @racket[(_ e* ...)] matches
 lists with at least one element.@note{The name @racket[e*] is our own convention.}
All but the first element of such a list is then bound to the identifier
 @racket[e*] in the result.
We use @racket[...] in the result to flatten the contents of @racket[e*] into
 the final expression.

One last example transformation using @racket[make-alias]
 is our definition of @racket[vector-ref], shown below.
When given a sized vector @racket[v] and an expression @racket[e] that
 expands to a number @racket[i], the function asserts that @racket[i] is
 in bounds.
If either @racket[vector/length] or @racket[expr->num] fail to coerce numeric
 values, the function returns @racket[#false].

@codeblock{
  (make-alias #'vector-ref
    (syntax-parser
     [(_ v:vector/length e)
      (let ([i (expr->num #'e)])
        (if i
          (if (< i (syntax->datum #'v.evidence))
            #`(unsafe-vector-ref v.expanded '#,i)
            (raise-vector-bounds-error #'v i))
          #false))]
     [_ #false]))
}

Unlike the previous two functions, our @racket[vector-ref] transformation
 does more than just matching a pattern and returning a new syntax object.
Crucially, it compares the @emph{value} used to index its argument vector with
 that vector's length before choosing how to expand.
To access these integer values outside of a template, we lift the pattern variables
 @racket[v] and @racket[e] to syntax objects with a @tt{#'} prefix.
A helper function @racket[expr->num] then fully expands the syntax object @racket[#'e]
 and the built-in @racket[syntax->datum] gets the integer value stored at the
 attribute @racket[#'v.evidence].

Programming in this style is similar to the example-driven explanations we gave
 in @Secref{sec:usage}.
The interesting design challenge is making one pattern that covers all
 relevant cases and one algorithm to uniformly derive the correct result.


@; =============================================================================
@section[#:tag "sec:impl-interp"]{Implementing Interpretations} @; TODO a decidedly bad name

By now we have seen two useful syntax classes: @racket[id] and @racket[vector/length].
In fact, we use syntax classes as the front-end for each function in @exact{$\interp$}.
@Figure-ref{fig:stxclass} lists all of our syntax classes and ties each to a purpose
 motivated in @Secref{sec:usage}.@note{The name @racket[vector/length] should
  be read as ``vector @emph{with} length information''.}

@figure["fig:stxclass"
  "Registry of syntax classes"
  @exact|{\input{fig-stxclass}}|
]

These classes are implemented uniformly from predicates on syntax objects.
One such predicate is @racket[arity?], shown below, which counts
 the parameters accepted by an uncurried anonymous function and returns
 @racket[#false] for all other inputs.

@codeblock{
  (define arity?
    (syntax-parser #:literals (λ)
     [(λ (x*:id ...) e* ...)
      (length (syntax->datum #'(x* ...)))]
     [_ #f]))
}

The syntax class @racket[procedure/arity] is then defined as ...

@racketblock[
> (define-stxclass/pred procedure/arity
    arity?)
]

... in terms of another macro, which handles the routine work of recursively
 expanding its input, applying the @racket[arity?] predicate,
 and caching results in the @racket[evidence] and @racket[expanded] attributes.

@codeblock{
  (define-syntax-rule (define-stxclass/pred id p?)
    (define-syntax-class id
     #:attributes (evidence expanded)
     (pattern e
      #:with e+ (expand-expr #'e)
      #:with p+ (p? #'e+)
      #:when (syntax->datum #'p+)
      #:attr evidence #'p+
      #:attr expanded #'e+)))
}

A @racket[define-syntax-rule] is an inlined definition; using it here does not
 save any space, but in practice we re-use the same alias for each of
 our custom syntax classes.
The @racket[#:attributes] declaration is very important.
This is where the earlier-mentioned @racket[v.expanded] and @racket[v.evidence]
 were defined, and indeed these two attributes form the backbone of our value-parsing
 protocol.
In terms of a pattern @racket[x:procedure/arity], their meaning is:
@itemlist[
  @item{
    @racket[x.expanded] is the result of fully expanding all macros and transformations
     contained in the syntax object bound to @racket[x].
    The helper function @racket[expand-expr] triggers this depth-first expansion.
  }
  @item{
    @racket[x.evidence] is the result of applying the @racket[arity?] predicate
     to the expanded version of @racket[x].
    Intuitively, @racket[x.evidence] is the reason why we should be able to
     perform transformations using @racket[x].
  }
]

If the predicate @racket[arity?] returns @racket[#false], then the boolean
 @racket[#:when] guard fails because the value contained in the syntax object
 @racket[p+] will be @racket[#false].
When this happens, neither attribute is bound and the pattern
 @racket[x.procedure/arity] will fail.

@; =============================================================================
@section{Implementing Definitions}

With that, we have essentially finished our tour of the key ideas underlying
 our implementation.
The one detail we elided is precisely how interpreted data is propogated upward
 through recursive transformations, especially since transformations may unfold
 into arbitrary, difficult-to-parse code.

An illustrative example is our transformation for @racket[sql-connect],
 the library function for connecting a user to a database.
Recall that our library imposes an extra constraint on calls to @racket[sql-connect]:
 they must supply a database schema, which is erased in translation.

@racketblock[
  (syntax-parser
   [(_ s:schema/spec e* ...)
    (syntax-property
      #'(sql-connect e* ...)
      connection-key
      #'s.evidence)]
   [_ (raise-syntax-error 'sql-connect
        "Missing schema")])
]

Most of this definition is routine.
We use the syntax class @racket[schema/spec] to lift schema specifications to
 the compile-time environment and we ultimately forward all non-schema arguments
 to the default @racket[sql-connect].@note{If an one of the arguments
   @racket[e* ...] is malformed, this will be reported by the original
   @racket[sql-connect]. Three cheers for division of labor!}
The new form is @racket[syntax-property], which tags our new syntax object
 with a key/value pair.
Here the key is @racket[connection-key], which we generate when compiling a file
 and use to identify connection objects.
The value is the evidence parsed from the schema description.

Transformation writers must take care to install @racket[syntax-property]
 information, but we automate the task of retrieving cached properties
 in our syntax classes---before
 applying a predicate, we first search for a cached value.
Syntax properties are likewise the trick for propagating metadata through
 @racket[let] and @racket[define] bindings.
The technical tools for this are @racket[rename-transformer]s and @racket[free-id-table]s,
 which we discuss in @Secref{sec:rename}.

@;@codeblock{
@;  (make-alias #'vector-append
@;    (syntax-parser
@;     [(_ v0:vector/length v1:vector/length)
@;      (define len0 (syntax-e #'v1.evidence))
@;      (define len1 (syntax-e #'v2.evidence))
@;      (define new-len (+ len0 len1))
@;      (syntax-property
@;        #`(build-vector
@;              #,new-len
@;              (lambda (i)
@;                (if (< i '#,len0)
@;                  (unsafe-vector-ref v1.expanded i)
@;                  (unsafe-vector-ref v2.expanded i)))
@;        vector-length-key
@;        new-len))]
@;     [_ #f]))
@;}


@; -----------------------------------------------------------------------------
@section{Ode to Macros: Greatest Hits}

@; Symphony of features

Whereas the previous section was a code-first tour of key techniques supporting
 our implementation, this section is a checklist of important meta-programming
 tools provided by the Racket macro system.
For ease of reference, our discussion proceeds from the most useful tool to
 the least.
Each sub-section title is a technical term.
@;Titles marked with an asterisk are essential to our implementation,
@; others could be omitted without losing the essence.


@subsection[#:tag "sec:parse"]{Syntax Parse}

The @racket[syntax/parse] library@~cite[c-dissertation-2010] is a powerful
 interface for writing macros.
It provides the @racket[syntax-parse] and @racket[syntax-parser] forms that
 we have used extensively above.
From our perspective, the key features are:
@itemlist[
  @item{
    A rich pattern-matching language; including, for example,
     repetition via @racket[...], @racket[#:when] guards, and matching
     for identifiers like @racket[λ] (top of @Secref{sec:impl-interp})
     that respects @exact{$\alpha$}-equivalence.
  }
  @item{
    Freedom to mix arbitrary code between the pattern spec and result,
     as shown in the definition of @racket[vector-ref]
     (bottom of @Secref{sec:impl-trans}).
  }
]


@subsection[#:tag "sec:local-expand"]{Depth-First Expansion}

Racket's macro expander normally proceeds in a breadth-first manner, traversing
 an AST top-down until it finds a macro in head position.
After expansion, sub-trees are traversed and expanded.
This ``lazy'' sort of evaluation is normally useful because it lets macro
 writers specify source code patterns instead of forcing them to reason about
 the shape of expanded code.

Our transformations, however, are most effective when value information is
 propogated bottom up from macro-free syntactic values through other combinators.
This requires depth-first macro expansion; for instance, in the first argument
 of the @racket[vector-ref] transformation defined in @Secref{sec:impl-trans}.
Fortunately, we always know which positions to expand depth-first and
 Racket provides a function @racket[local-expand] that will fully expand
 a target syntax object.
In particular, all our syntax classes listed in @Figure-ref{fig:stxclass}
 locally expand their target.


@subsection[#:tag "sec:class"]{Syntax Classes}

A syntax class encapsulates common parts of a syntax pattern.
With the syntax class shown at the end of @Secref{sec:impl-interp}
 we save 2-6 lines of code in each of our transformation functions.
More importantly, syntax classes provide a clean implementation of the ideas
 in @Secref{sec:solution}.
Given a function in @exact{$\interp$} that extracts data from core value/expression
 forms, we generate a syntax class that applies the function and handles
 intermediate binding forms.
Functions in @exact{$\trans$} can branch on whether the syntax class matched
 instead of parsing data from program syntax.


@subsection[#:tag "sec:idmacro"]{Identifier Macros}

Traditional macros may appear only in the head position of an expression.
For example, the following are illegal uses of the built-in @racket[or]
 macro:
@racketblock[
> or
or: bad syntax
> (map or '((#t #f #f) (#f #f)))
or: bad syntax
]

Identifier macros are allowed in both higher-order and top-level positions,
 just like first-class functions.
This lets us transparently alias built-in functions like @racket[regexp-match]
 and @racket[vector-length] (see @Secref{sec:impl-trans}).
The higher-order uses cannot be checked for bugs, but they execute as normal
 without raising new syntax errors.


@subsection[#:tag "sec:def-implementation"]{Syntax Properties}

Syntax properties are the glue that let us chain transformations together.
For instance, @racket[vector-map] preserves the length of its argument vector.
By tagging calls to @racket[vector-map] with a syntax property, our system
 becomes aware of identities like:

@centered[
  @codeblock{
    (vector-length v) == (vector-length (vector-map f v))
  }
]

Furthermore, syntax properties place few constraints on the type of data
 used as keys or values.
This proved useful in our implementation of @racket[query-row], where we stored
 an S-expression representing a database schema in a syntax property.


@subsection[#:tag "sec:rename"]{Rename Transformers, Free Id Tables}

Cooperating with @racket[let] and @racket[define] bindings is an important
 usability concern.
When testing this library on existing code, we often saw code like:

@(begin
#reader scribble/comment-reader
(racketblock
(define rx-email #rx"^(.*)@(.*)\\.(.*)$")

;; Other code here

(define (get-recipient str)
  (regexp-match rx-email str))
))

Similarly for database code and arithmetic constants.

To deal with @racket[let] bindings, we use a @racket[rename-transformer].
Within the binding's scope, the transformer redirects references from a variable
 to an arbitrary syntax object.
For our purposes, we redirect to an annotated version of the same variable:

@racketblock[
> '(let ([x 4])
     (+ x 1))
'(let ([x 4])
  (let-syntax ([x (make-rename-transformer #'x
                    secret-key 4)])
    (+ x 1)))
]

For definitions, we use a @emph{free-identifier table}.
This is less fancy--just a hashtable whose keys respect @exact{$\alpha$}-equivalence--but
 still useful in practice.


@subsection[#:tag "sec:phase"]{Phasing}

Any code between a @racket[syntax-parse] pattern and the output syntax object
 is run at compile-time to generate the code that is ultimately run.
In general terms, the code used to directly generate run-time code
 executes at @emph{phase level} 1 relative to the enclosing module.
Code used to generate a syntax-parse pattern may be run at phase level 2, and
 so on up to as many phases as needed@~cite[f-icfp-2002].

Phases are explicitly separated.
By design, it is difficult to share state between two phases.
Also by design, it is very easy to import bindings from any module at a specific
 phase
The upshot of this is that one can write and test ordinary, phase-0 Racket code
 but then use it at a higher phase level.
@; + non-meta programming
@; + not getting stuck in ascending ladder
@; + modular development


@subsection{Lexical Scope, Source Locations}

Perhaps it goes without saying, but having macros that respect lexical scope
 is important for a good user and developer experience.
Along the same lines, the ability to propogate source code locations in
 transformations lets us report syntax errors in terms of the programmer's
 source code rather than locations inside our library.


