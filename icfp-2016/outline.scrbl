#lang scribble/manual

@(require "common.rkt")

@title{Outline}
@; Applications

(Our goal is not to discourage dependent types, but rather to advertise macros.)
(Jack Firth might be using the library. Email him?)

Each application is works by identifying common properties of a set of
 values and using these properties in a macro transformation.

@itemlist[
  @item{@tt{typeof : (-> Syntax TypeTag)}}
  @item{@tt{transformations : (-> TypeTag (Set (-> Syntax Syntax))}}
]

The following table defines type tags and related @tt{transformations}.

@exact|{
\begin{tabular}{l l l l l}
  Type          &    Syntax Category          &    Metadata    &    Applications    \\\hline\hline
\\Format String &       {\tt string}          &     \#/$\tau$ of fmt chars & Check args to {\tt printf}
\\Regexp        &  {\tt string} / {\tt bytes} & \# groups & Check paren-matching${}^*$, smarter return type
\\Query String  &  {\tt string}               & \# of {\tt \$} vars & Check arity
\\Function      &  {\tt (lambda ...)}         & Arity        & Type for {\tt apply}
\\Numbers       &  {\tt number}               & Value & Constant folding, accurate types
\\Vectors       &  {\tt '\#( ... )}            & Length & Access, size, fast operations
\end{tabular}
}|

Caveats / Extras:
@itemlist[
  @item{Regular expressions should work with partial groups and warn about unvalidated user input.
        i.e. @tt{#rx"prefix (" + #rx"mid" + #rx") suffix"} should work}
  @item{Will hijack @tt{define} to get procedure arity}
  @item{Add smarter runtime assertions, like @tt{assert-length} and a remembering @tt{index?}}
]


@;@section{String DSLs}
@;Predicate is @tt{string?}.
@;
@;@subsection{format}
@;Format strings contain format characters.
@;Type tag is @tt{string:format}.
@;Transformations are @tt{format} and @tt{printf}.
@;
@;Analysis parses format characters and determines the number & type
@; of remaining arguments.
@;
@;@;@exact|{$\Pi (x:String) . FormatType(x) \rightarrow String$}|
@;
@;
@;@subsection{regexp-match}
@;Regular expressions have primitive syntax, but any string or byte string
@; is also a valid regular expression.
@;Type tag is @tt{string:regexp}
@;Transformation is @tt{regexp-match}.
@;
@;Analysis parses groups.
@;Looks for matched parentheses and counts the number of groups.
@;
@;
@;@subsection{database}
@;Query strings recognized by the @racket[db] library.
@;Type tag is @tt{string:query}.
@;The dollar sign (@tt{$}) is for DB variables.
@;Transformations are query functions.
@;
@;Analysis matches arguments with number of DB variables.
@;
@;TODO: database connection needs to carry row information
@;
@;
@;@section{arithmetic}
@;In Typed Racket, @tt{0} has type @tt{Zero} but @tt{(- 5 5)} has type @tt{Integer}.
@;We fix this.
@;
@;Type tag is @tt{number}.
@;Predicate is @tt{number?}
@;Transformations are basic arithmetic @tt{+ - * /}.
@;
@;Analysis folds constants, so @tt{(- 5 5)} is @tt{0} at runtime.
@;
@;TODO: attach actual values to variables, remember that i.e. @tt{5} can
@; be @tt{sub1}'d 5 times before changing type to @tt{Integer}.
@;Can we do the same for @tt{Index} and @tt{add1}?
@;
@;
@;@section{map}
@;For variable-arity polymorphism.
@;
@;Type tag is @tt{procedure}.
@;Predicate is @tt{procedure?}
@;Transformations are functions like @tt{map} and @tt{apply}.
@;
@;Analysis matches known arities with given argument counts.
@;
@;Not sure how this improves over polydots, except for @tt{apply}.
@;
@;
@;@section{sized vectors}
@;
@;Track length of vectors.
@;Type tag is @tt{vector}.
@;Predicate is @tt{vector?}.
@;
@;Analysis:
@;@itemlist[
@;  @item{Faster versions of code}
@;  @item{Propogate length through functions like append}
@;  @item{Catch index out-of-bounds}
@;]
