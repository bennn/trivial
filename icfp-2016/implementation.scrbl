#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:implementation"]{More than a Pretty Face}
@; Amazing Macros
@; Whirlwind tour
@; Accounting, taking stock
@; Alas I knew him well


@; TODO this is all so boring right now, need to revise heavily

@Figure-ref{fig:stats} gives a few statistics regarding our implementation.
The purpose of this section is to explain why the line counts are low.
@; Ode to macros, implementation a symphony

@figure["fig:stats"
  "Quantifying the implementation"
  @exact|{\input{fig-stats}}|
]

In total, the code for our six applications described in @Secref{sec:usage}
 comprise 901 lines of code (LOC).
Another 145 lines implement common functionality, putting the grand total
 just over 1000 LOC.

Except for @exact|{\mod{db}}| and @exact|{\mod{regexp}}|, each of the
 core modules defines a single interpretation function (in @exact|{$\interp$}|).
In @exact|{\mod{db}}|, the two functions are the schema predicate and @tt{sql}
 query parser.
In @exact|{\mod{regexp}}|, we have six group-parsing functions
 to match the six string-like input types
   @;@note{Strings, Regex literals, Posix Regex literls, and byte-string variations of each.}
 accepted by Racket's @racket[regexp-match].
These group parsers, however, share a 33-line kernel.
Incidentally, the average size of all value-interpreting functions is 33 LOC.
The smallest interpreter is the composition of Racket's @racket[number?] predicate
 with the identity interpretation in @exact|{\mod{math}}| (3 LOC).
The largest is the query parser (35 LOC), though the analyses for
 format strings and regular expressions are approximately the same size.

The elaboration functions are aliases for standard library procedures.
Typically, these functions match a syntactic pattern, check for value errors,
 and elaborate to a specialized Typed Racket procedure call.
All these tasks can be expressed concisely; the average size of a function in
 @exact{$\elab$} is 10 lines and the median is 7 lines.
Much of the brevity is due to amortizing helper functions, so we include helpers'
 line counts in the figure.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:impl-elab"]{Elegant Elaborations}

Our elaboration for @racket[vector-length] is straightforward.
If called with a size-annotated vector @racket[v], @racket[(vector-length v)]
 elaborates to the size.
Otherwise, it defaults to Typed Racket's @racket[vector-length].
The implementation is equally concise, modulo some notation.

@codeblock{
  (make-alias #'vector-length
    (syntax-parser
     [(_ v:vector/length)
      #''v.evidence]
     [_ #false]))
}


@itemlist[
  @item{
    @racket[(make-alias id f)] creates a elaboration from an identifier @racket[id]
    and a partial function @racket[f].
  }
  @item{
    The symbol @exact|{\RktMeta{\#'}}| creates a syntax object from a value or template.
  }
  @item{
    A @racket[syntax-parser] is a match statement over syntactic patterns.
    This parser recognizes two cases: application to a single argument via
     the pattern @racket[(_ v:vector/length)] and anything else with the
     wildcard @tt{_}.
  }
  @item{
    The colon character (@tt{:}) used in @racket[v:vector/length]
     binds the variable @racket[v] to the @emph{syntax class} @racket[vector/length].
  }
  @item{
    The dot character (@tt{.}) accesses an @emph{attribute} of the value bound
     to @racket[v].
    In this case, the attribute @racket[evidence] is set when the class
     @racket[vector/length] successfully matches the value of @racket[v].
  }
]

The pattern @racket[v:vector/length] unfolds all
 elaborations to @racket[v] recursively.
So, as hinted in @Secref{sec:vector}, 
 we handle each of the following cases as well as any other combination of
 length-preserving vector operations.

@racketblock[
> (vector-length #(0 1 2))
2
> (vector-length (vector-append #(A B)
                                #(C D)))
4
]

@; The general features are explained in greater deteail below

@; make-alias
The structure of @racket[vector-length] is common to many of our elaborations:
we define a rule to handle an interesting syntactic pattern and
 then generate an alias from the rule using the helper function @racket[make-alias].

@codeblock{
  (define ((make-alias orig-id elaborate) stx)
    (or (elaborate stx)
        (syntax-parse stx
         [_:id
          orig-id]
         [(_ e* ...)
          #`(#,orig-id e* ...)])))
}

The elaboration defined by @racket[(make-alias id elaborate)] is a function on
 syntax objects.
This function first applies @racket[elaborate] to the syntax object @racket[stx].
If the result is not @racket[#false] we return.
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
     we declare a syntax template with @exact|{\RktMeta{\#`}}| and splice the identifier
     @racket[orig-id] into the template with @exact|{\RktMeta{,\#}}|.
    These operators are formally known as @racket[quasisyntax] and @racket[unsyntax];
     you may know their cousins @racket[quasiquote] and @racket[unquote].
  }
]

The identifier @racket[...] is not pseudocode.
In a pattern, it captures zero-or-more repetitions of the preceding pattern---in
 this case, the variable @racket[e*] binds anything so @racket[(_ e* ...)] matches
 lists with at least one element.@note{The variable name @racket[e*] is our own convention.}
All but the first element of such a list is then bound to the identifier
 @racket[e*] in the result.
We use @racket[...] in the result to flatten the contents of @racket[e*] into
 the final expression.

A second example using @racket[make-alias]
 is @racket[vector-ref] @exact{$\in \elab$}, shown below.
When given a sized vector @racket[v] and an expression @racket[e] that
 expands to a number @racket[i], the function asserts that @racket[i] is
 in bounds.
If either @racket[vector/length] or @racket[expr->num] fail to coerce numeric
 values, the function defaults to Typed Racket's @racket[vector-ref].

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

This elaboration
 does more than just matching a pattern and returning a new syntax object.
Crucially, it compares the @emph{value} used to index its argument vector with
 that vector's length before choosing how to expand.
To access these integer values outside of a template, we lift the pattern variables
 @racket[v] and @racket[e] to syntax objects with a @exact|{\RktMeta{\#'}}| prefix.
A helper function @racket[expr->num] then fully expands the syntax object @racket[#'e]
 and the built-in @racket[syntax->datum] gets the integer value stored at the
 attribute @racket[#'v.evidence].

Programming in this style is similar to the example-driven explanations we gave
 in @Secref{sec:usage}.
The interesting design challenge is making one pattern that covers all
 relevant cases and one algorithm to uniformly derive the correct result.


@; =============================================================================
@section[#:tag "sec:impl-interp"]{Illustrative Interpretations}

Both @racket[id] and
 @racket[vector/length] are useful syntax classes.@note{The name @racket[vector/length] should
  be read as ``vector @emph{with} length information''.}
They recognize syntax objects with certain well-defined properties.
In fact, we use syntax classes as the front-end for each function in @exact{$\interp$}.
@Figure-ref{fig:stxclass} lists all of our syntax classes and ties each to a purpose
 motivated in @Secref{sec:usage}.

@figure["fig:stxclass"
  "Registry of syntax classes"
  @exact|{\input{fig-stxclass}}|
]

The definitions of these classes are generated from predicates on syntax
 objects.
One such predicate is @racket[vector?], shown below, which counts the
 length of vector values and returns @racket[#false] for all other inputs.
Notice that the pattern for @racket[make-vector] recursively expands its
 first argument using the @racket[num/value] syntax class.

@codeblock{
  (define vector?
    (syntax-parser #:literals (make-vector)
     [#(e* ...)
      (length (syntax->datum #'(e* ...)))]
     [(make-vector n:num/value)
      (syntax->datum #'n.evidence)]
     [_ #f]))
}


From @racket[vector?], we define the syntax class @racket[vector/length]
 that handles the mechanical work of macro-expanding its input,
 applying the @racket[vector?] predicate, and caching results in the
 @racket[evidence] and @racket[expanded] attributes.

@codeblock{
  (define-syntax-class vector/length
   #:attributes (evidence expanded)
   (pattern e
    #:with e+  (expand-expr #'e)
    #:with len (vector? #'e+)
    #:when (syntax->datum #'len)
    #:attr evidence #'len
    #:attr expanded #'e+))
}

The @racket[#:attributes] declaration is key.
This is where the earlier-mentioned @racket[v.expanded] and @racket[v.evidence]
 properties are defined, and indeed these two attributes form the backbone
 of our protocol for cooperative elaborations.
In terms of a pattern @racket[v:vector/length], their meaning is:
@itemlist[
  @item{
    @racket[v.expanded] is the result of fully expanding all macros and elaborations 
     contained in the syntax object bound to @racket[v].
    The helper function @racket[expand-expr] triggers this depth-first expansion.
  }
  @item{
    @racket[v.evidence] is the result of applying the @racket[vector?] predicate
     to the expanded version of @racket[v].
    In general, @racket[v.evidence] is the reason why we should be able to
     perform elaborations using the value bound to @racket[v].
  }
]

If the predicate @racket[vector?] returns @racket[#false] then the boolean
 @racket[#:when] guard fails because the value contained in the syntax object
 @racket[len] will be @racket[#false].
When this happens, neither attribute is bound and the pattern
 @racket[v.vector/length] will fail.


@; =============================================================================
@section{Automatically Handling Variables}

When the results of an elaboration are bound to a variable @racket[v],
 we frequently need to associate a compile-time value to @racket[v] for
 later elaborations to use.
This is often the case for calls to @racket[sql-connect]:

@racketblock[
(define C (sql-connect ....))
(query-row C ....)
]

Reading the literal variable @racket[C] gives no useful information
 when elaborating the call to @racket[query-row].
Instead, we need to retrieve the database schema for the connection bound to @racket[C].

The solution starts with our implementation of @racket[sql-connect],
 which uses the built-in function
 @racket[syntax-property] to associate a key/value pair with a syntax object.
Future elaborations on the syntax object @racket[#'(sql-connect e* ...)] can
 retrieve the database schema @racket[#'s.evidence] by using the key @racket[connection-key].

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

Storing this syntax property is the job of the programmer, but we automate
 the task of bubbling the property up through variable definitions by overriding
 Typed Racket's @racket[define] and @racket[let] forms.
New definitions search for specially-keyed properties like @racket[connection-key];
 when found, they associate their variable with the property in a local hashtable
 whose keys are @exact{$\alpha$}-equivalence classes of identifiers.
New @racket[let] bindings work similarly, but redirect variable references
 within their scope.
The technical tools for implementing these associations are @racket[free-id-table]s
 and @racket[rename-transformer]s (@Secref{sec:rename}).


@; -----------------------------------------------------------------------------
@section{Ode to Macros: Greatest Hits}

@; Symphony of features

This section is a checklist of important meta-programming
 tools provided by the Racket macro system.
Each sub-section title is a technical term;
 for ease of reference, our discussion proceeds from the most useful tool to
 the least.
@;Titles marked with an asterisk are essential to our implementation,
@; others could be omitted without losing the essence.


@subsection[#:tag "sec:parse"]{Syntax Parse}

The @racket[syntax/parse] library@~cite[c-dissertation-2010] provides
 tools for writing macros.
It provides the @racket[syntax-parse] and @racket[syntax-parser] forms that
 we have used extensively above.
From our perspective, the key features are:
@itemlist[
  @item{
    A rich pattern-matching language; including, for example,
     repetition via @racket[...], @racket[#:when] guards, and matching
     for identifiers like @racket[make-vector] (top of @Secref{sec:impl-interp})
     that respects @exact{$\alpha$}-equivalence.
  }
  @item{
    Freedom to mix arbitrary code between the pattern spec and result,
     as shown in the definition of @racket[vector-ref]
     (bottom of @Secref{sec:impl-elab}).
  }
]


@subsection[#:tag "sec:local-expand"]{Depth-First Expansion}

Racket's macro expander normally proceeds in a breadth-first manner, traversing
 an AST top-down until it finds a macro in head position.
After expansion, sub-trees are traversed and expanded.
This ``lazy'' sort of evaluation is normally useful because it lets macro
 writers specify source code patterns instead of forcing them to reason about
 the syntax trees of expanded code.

Our elaborations, however, are most effective when value information is
 propogated bottom up from macro-free syntactic values through other combinators.
This requires depth-first macro expansion; for instance, in the first argument
 of the @racket[vector-ref] elaboration defined in @Secref{sec:impl-elab}.
Fortunately, we always know which positions to expand depth-first and
 Racket provides a function @racket[local-expand] that will fully expand
 a target syntax object.
In particular, all our syntax classes listed in @Figure-ref{fig:stxclass}
 locally expand their target.


@subsection[#:tag "sec:class"]{Syntax Classes}

A syntax class encapsulates common parts of a syntax pattern.
With the syntax class shown at the end of @Secref{sec:impl-interp}
 we save 2-6 lines of code in each of our elaboration functions.
More importantly, syntax classes provide a clean implementation of the ideas
 in @Secref{sec:solution}.
Given a function in @exact{$\interp$} that extracts data from core value/expression
 forms, we generate a syntax class that applies the function and handles
 intermediate binding forms.
Functions in @exact{$\elab$} can branch on whether the syntax class matched
 instead of parsing data from program syntax.

In other words, syntax classes provide an interface that lets us reason
 locally when writing elaborators.
The only question we ask during elaboration is whether a syntax object is associated
 with an interpreted value---not how the object looks or what sequence of
 renamings it filtered through.


@subsection[#:tag "sec:idmacro"]{Identifier Macros}

Traditional macros may appear only in the head position of an expression.
For example, the following are illegal uses of the built-in @racket[or]
 macro:

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{or}

\evalsto\RktErr{or: bad syntax}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{map}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{or}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{(}\RktVal{(}\RktVal{\#t}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{\#f}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{\#f}\RktVal{)}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{(}\RktVal{\#f}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{\#f}\RktVal{)}\RktVal{)}\RktPn{)}

\evalsto\RktErr{or: bad syntax}
\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Identifier macros are allowed in both higher-order and top-level positions,
 just like first-class functions.
This lets us transparently alias built-in functions like @racket[regexp-match]
 and @racket[vector-length] (see @Secref{sec:impl-elab}).
The higher-order uses cannot be checked for bugs, but they execute as normal
 without raising new syntax errors.


@subsection[#:tag "sec:def-implementation"]{Syntax Properties}

Syntax properties are the glue that let us compose elaborations.
For instance, @racket[vector-map] preserves the length of its argument vector.
By tagging calls to @racket[vector-map] with a syntax property, our system
 becomes aware of identities like:

@centered[
  @codeblock{
    (vector-length (vector-map f v))
      ==
    (vector-length v)
  }
]

Furthermore, syntax properties place few constraints on the type of data
 used as keys or values.
This proved useful in our implementation of @racket[query-row], where we stored
 an S-expression representing a database schema in a syntax property.


@subsection[#:tag "sec:rename"]{Rename Transformers, Free Id Tables}

Cooperating with @racket[let] and @racket[define] bindings is an important
 usability concern.
To deal with @racket[let] bindings, we use a @racket[rename-transformer].
Within the binding's scope, the transformer redirects references from a variable
 to an arbitrary syntax object.
For our purposes, we redirect to an annotated version of the same variable:

@racketblock[
> ⟦'(let ([x 4])
      (+ x 1))⟧
==> '(let ([x 4])
      (let-syntax ([x (make-rename-transformer #'x
                        secret-key 4)])
        (+ x 1)))
]

For definitions, we use a @emph{free-identifier table}.
This is less fancy--it is just a hashtable whose keys respect
 @exact{$\alpha$}-equivalence--but still useful in practice.


@subsection[#:tag "sec:phase"]{Phasing}

Any code between a @racket[syntax-parse] pattern and the output syntax object
 is run at compile-time to generate the code that is ultimately run.
In general terms, code used to directly generate run-time code
 executes at @emph{phase level} 1 relative to the enclosing module.
Code used to generate a @racket[syntax-parse] pattern may be run at
 phase level 2, and
 so on up to as many phases as needed@~cite[f-icfp-2002].

Phases are explicitly separated.
By design, it is difficult to share state between two phases.
Also by design, it is very easy to import bindings from any module at a specific
 phase.
The upshot of this is that one can write and test ordinary, phase-0 Racket code
 but then use it at a higher phase level.
We also have functions like @racket[+] available at whatever stage of
 macro expansion we should need them---no need to copy and paste the implementation
 at a different phase level@~cite[ew-haskell-2012].


@subsection{Lexical Scope, Source Locations}

Perhaps it goes without saying, but having macros that respect lexical scope
 is important for a good user and developer experience.
Along the same lines, the ability to propogate source code locations in
 elaborations lets us report syntax errors in terms of the programmer's
 source code rather than locations inside our library.
Even though we may implement complex transformations, errors can always be
 traced to a source code line number.

