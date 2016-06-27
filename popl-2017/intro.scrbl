#lang scribble/sigplan @onecolumn

@require["common.rkt" (only-in scribble/base nested) "bib.rkt"]

@title[#:tag "sec:intro"]{Type Eleborators Need APIs}

Type systems such as ML's, Haskell's, or Scala's help developers convey
their thinking to their successors in a sound and concise manner.  The
implementations of such type systems tend to employ elaboration passes to
check the types of a specific program. As an elaborator traverses the
(parsed) synatx, it simultaneously reconstructs types, checks their
consistency according to the underlying type theory, replaces many of the
surface-syntax constructs with constructs from the kernel language, and
inserts type information to create an annotated representation.

Some programming languages also support a meta-API, that is, a way to
programmatically direct the elaborator. For example, Rust and Scala come
with compiler plug-ins. Typed Racket and Types Clojure inherit the macro
mechanisms of their parents. This paper refers to all such mechanisms as
@defterm{elaborator API}s.

Equipping type-checking elaborators with APIs---or using the existing
APIs---promises new ways to expand the power of type checking at a
relatively low cost. Specifically, the developers of libraries can
@defterm{tailor} typing rules to the exported functions and linguistic
constructs.

Consider the example of tailoring the API of a library that implements a
string-based, domain-specific languages. Examples of such libraries abound:
formatting, regular-expression matching, database queries, and so on.  The
creators of these DSLs tend to know a lot about how programs written in
these DSLs relate to the host program, but they (usually) cannot express
this knowledge in API types. Type tailoring allows such programmers to
refine the typing rules for the APIs in a relatively straightforward
way. Recall the API of the regular-expression library. In a typical typed
language, the matching function is exported with a type like this:
@;
@exact|{
\begin{mathpar}
\mbox{regexp-match} : \mbox{String} \rightarrow \mbox{Opt[Listof[String]]}
\end{mathpar}\hspace{-.3em}}|
@; the above is an ***incredibly disgusting hack to work around a scribble bug***
When the result is a list of strings, the length of the list depends on the
argument string---but the API for the regular-expression library cannot
express this relationship between the regular expression and the
surrounding host program. As a result, the author of the host program must
inject additional complexity, often in the form of (hidden) run-time checks.

If type tailoring is available, the creator of the library can refine the
type of @tt{regexp-match} with rules such as this one:
@;
@exact|{
\begin{mathpar}
  \inferrule*[]{
    \elabsto{e}{e'}{\mbox{String}}, \\\\
     \mbox{\it Program} \vdash e' \mbox{ does not contain a grouping}
  }{
    \elabsto{\mbox{regexp-match}~e}{\mbox{regexp-match}~e'}{\mbox{Opt[List[String]]}}
  }
\end{mathpar}\hspace{-.3em}}|
That is, when the extended elaborator can use (a fragment of) the program
to prove that the given string for a specific use of @tt{regexp-match}
does not contain grouping specifications---ways to extract a sub-string via
matching---the @tt{Some} result type is a list of a single
string. The original specifications remains the default rule, which the
type checker uses when it cannot use the specific ones. 

A similar rule would say if the string contains exactly one such grouping
(and no alternative), a match produces a list of two strings. Critically,
this refined type for the result of applying @tt{regexp-match} enables
further type refinements, just like constant folding enables additional
compiler optimizations. In this specific case, the program context of the
application of @tt{regexp-match} may dereference the list with unsafe---and
thus faster---versions of @tt{first} and @tt{second} once it has confirmed
a match.

Vectors offer a similar opportunity for unsafe dereferencing via 
type tailoring. Typically, such a library exports a dereferencing construct with
the following type rule: 
@exact|{
\begin{mathpar}
  \inferrule*[]{
    \elabsto{e_1}{e_1'}{\tarray}
    \\
    \elabsto{e_2}{e_2'}{\tint}
  }{
    \elabsto{{e_1}[{e_2}]}{\checkedref~e_1'~e_2'}{\tint}
  }
\end{mathpar}\hspace{-.3em}}|
@;
If the elaborator can prove, however, that the indexing expression @tt{e}
is equal to or evaluates to a specific integer @tt{i}, the rule can be
strengthened as follows: 
@;
@exact|{
\begin{mathpar}

  \inferrule*[]{
    \elabsto{e_1}{\vectorvn}{\tarray},
    \\
    \elabsto{e_2}{e'_2}{\tint}, 
    \\\\
    \mbox{\it Program} \vdash e'_2 = i, \quad
    i \in \ints, \quad
    0 \le i < n
  }{
    \elabsto{{e_1}[{e_2}]}{\unsaferef~\vectorvn~i}{\tint}
  }
\end{mathpar}\hspace{-.3em}}|

This paper introduces and evaluates the novel idea of type tailoring. It
uses Typed Racket and its API to the elaborator (section 2) because the
language already supports appropriate type and run-time systems and because
it is relatively straightforward to program the type
elaborator---@emph{without modifying it}.  To make type tailoring concrete
and to demonstrate its usefulness, the paper presents two case studies
(sections 3 and 4). Each report on a case study consists of three parts: a
type soundness argument assuming a type soundness argument for the complete
language exists; the actual
@;
@margin-note*{BEN: this evaluation is missing an idea}
@;
implementation; and an evaluation that reports how often the revised type
elaborator can improve the code. The first type tailoring---chosen for the
wide familiarity of the domain---enriches the type-checking of
vector-referencing operations (section 3). The second example explains how
the implementor of a string-based embedded DSL---a regexp-matching DSL,
also widely familiar to programming researchers---may tailor the types of
the interpretation function (section 4).  In addition, the paper sketches
several other applications of type tailoring in Typed Racket (section
5). The final two sections compare programmability of the type elaborator
to work on dependent types and sketch how such an API could be implemented
for other languages.
