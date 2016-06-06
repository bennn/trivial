#lang scribble/sigplan @onecolumn

@require["common.rkt"]

@title[#:tag "sec:segfault"]{Well Typed Programs do not go SEGFAULT}

@Figure-ref{fig:stlc} describes a simply typed @exact{$\lambda$} calculus
 with integers and integer arrays.
@Figure-ref{fig:elab0} gives a type-directed elaboration of terms written
 in this surface language to a typed, executable core language.
The main difference between surface and core is that array references
 @exact{$\aref{e}{e}$} are replaced with calls to a primitive operation
 @exact{$\checkedref$}.
For example, the term @exact|{$\aref{x}{3}$}| elaborates to @exact|{$\checkedref~x~3$}|
 when @exact{$x$} is a variable with type @exact{$\tarray$}.

The operational semantics for the core language are given in @Figure-ref{fig:stlc-core},
 along with type judgments for two primitive operations:
 @exact{$\checkedref$} and @exact{$\unsaferef$}.
Intuitively, @exact{$\checkedref$} is a memory-safe function that performs a bounds check before
 dereferencing an array and gives a runtime error when called with incompatible values.
On the other hand, @exact{$\unsaferef$} does not perform a bounds check and therefore
 may return arbitrary data or raise a memory error when called with an invalid index.
These negative consequences are modeled in the (family of) rule(s) @exact|{\textsc{E-UnsafeFail}}|.
Consequently, the judgment @exact{$\smallstep{e}{e}$} and its transitive closure
 @exact{$\smallstepstar{e}{e}$} are relations, whereas
 typing and elaboration are proper functions.

 @figure["fig:stlc" "Simply-Typed λ Calculus" #:style left-figure-style
  @exact|{\input{fig-stlc-surface}}|
 ]

 @figure["fig:elab0" "Elaborating typed terms" #:style left-figure-style
   @exact|{\input{fig-elab0}}|
 ]

 @figure["fig:stlc-core" "Core Language" #:style left-figure-style
   @exact|{\input{fig-stlc-core}}|
 ]

Evaluation of the surface language in @Figure-ref{fig:stlc}, however,
 is deterministic.

  @definition["evaluation"]{
    @exact{$\bigstep{e}{v}$} if and only if @exact{$e$} is closed and there
     exists @exact{$e'$}, @exact{$\tau$} such that
     @exact|{$\elabstoclosed{e}{e'}{\tau}$}|
     and @exact{$\smallstepstar{e'}{v}$}.
  }

  @theorem["determinism"]{
    Evaluation is a proper function.
    In other words, if @exact{$\bigstep{e}{v}$}
     and @exact{$\bigstep{e}{v'}$}
     then @exact{$v = v'$}.
  }

  @proof{
    By induction on terms of the core language, if
     @exact{$\smallstep{e}{e'}$} and @exact{$e$} is not of the form
     @exact{$\unsaferef~e_1~e_2$} then @exact{$e'$} is unique.
    The theorem follows because the surface language does not allow
     @exact{$\unsaferef$} and elaboration does not introduce unsafe references.
  }

Determinism implies that evaluation never invokes the @sc{E-UnsafeFail} rule;
 therefore, evaluation of surface terms is memory safe.
Additionally, the surface language is type safe.

  @theorem["soundness"]{
    If @exact{$e$} is closed and @exact|{$\elabstoclosed{e}{e'}{\tau}$}|
     then one of the following holds:
    @itemlist[
      @item{
        @exact{$e'$} is a value
      }
      @item{
        @exact{$\smallstep{e'}{\indexerror}$}
      }
      @item{
        @exact{$\smallstep{e'}{e''}$}
        and @exact{$\typestoclosed{e''}{\tau}$}
      }
    ]
  }

  @proof{
    The interesting cases involve @exact{$\checkedref$}.
    First, @exact|{$\elabstoclosed{\aref{e_1}{e_2}}{\checkedref~e_1~e_2}{\tint}$}|
     implies @exact{$\typestoclosed{e_1}{\tarray}$}
     and @exact{$\typestoclosed{e_2}{\tint}$}.
    Depending on the values of @exact{$e_1$} and @exact{$e_2$},
     the application steps to either @exact{$\indexerror$} or the @exact{$e_2$}-th
     element of the array @exact{$e_1$}---which, by assumption, has type @exact{$\tint$}.
  }


@section{Extending the Elaborator}

Building on the foundation of type soundness, we extend the elaborator with
 the @emph{value-directed} rules in @Figure-ref{fig:elab1}.
When the elaborator can prove that an array reference is in bounds based on
 the syntax of terms, it produces an unsafe reference.
Conversely, the elaborator raises a compile-time index error if it can prove
 the index is outside the bounds of the array.

  @figure["fig:elab1" "Value-directed elaboration" #:style left-figure-style
    @exact|{\input{fig-elab1}}|
  ]

Our thesis is that adding such rules does not undermine the safety guarantees of the
 original language.
In this case, we recover the original guarantees by extending the proofs of
 type and memory safety to address the @sc{S-RefPass} and @sc{S-RefFail} elaboration rules.

  @definition[@exact{$\elabarrowplus$}]{
    The relation @exact{$\elabarrowplus$} is the union of the rules in
     @Figure-ref["fig:elab0" "fig:elab1"].
  }

  @theorem["determinism"]{
    @exact|{$\bigstepplus{e}{v}$}| is a proper function, where @exact|{$\bigstepplus{e}{v}$}|
     if and only if @exact|{$\elabstoclosedplus{e}{e'}{\tau}$}|
     and @exact|{$\smallstepstar{e'}{v}$}|.
  }

  @proof{
    With the original elaboration scheme, all array references @exact{$\aref{e_1}{e_2}$}
     elaborate to @exact{$\checkedref~e_1'~e_2'$}, where @exact{$e_1'$}
     and @exact{$e_2'$} are the elaboration of @exact{$e_1$} and @exact{$e_2$}.
    There are now three possibilities:
      @itemize[
        @item{
          @exact{$e_1'$} is an array value @exact{$\vectorvn$}
           and @exact{$e_2'$} is an integer value @exact{$i \in \ints$}
           and @exact{$0 \le i < n$}.
          Both @sc{S-Ref} and @sc{S-RefPass} are possible elaborations,
           therefore @exact{$e'$} is either @exact{$\checkedref~\vectorvn~i$}
           or @exact{$\unsaferef~\vectorvn~i$}.
          Because @exact{$0 \le i < n$}, evaluation must proceed with
           @sc{E-CheckPass} or @sc{E-UnsafePass} in each case, respectively.
          These rules have the same result, @exact{$v_i$}.
        }
        @item{
          @exact{$e_1'$} is an array value @exact{$\vectorvn$}
           and @exact{$e_2'$} is an integer value @exact{$i \in \ints$}
           and either @exact{$i < 0$} or @exact{$i \ge n$}.
          The rules @sc{S-Ref} and @sc{S-RefFail} are possible elaborations.
          If @sc{S-Ref} is chosen, @exact{$e'$} is @exact{$\checkedref~\vectorvn~i$}
           and evaluates to @exact{$\indexerror$} because @exact{$i$} is outside
           the bounds of the array.
          If @sc{S-RefFail} is chosen an index error is raised immediately.
        }
        @item{
          Otherwise, either @exact{$e_1'$} or @exact{$e_2'$} is not a value
           form and we rely on the existing proof of determinism.
        }
      ]
  }

  For practical purposes, non-determinism in the elaborator should be
   resolved giving the rules in @Figure-ref{fig:elab1} preference over the rules
   in @Figure-ref{fig:elab0}.
  But the result will be the same in any event.


  @theorem["soundness"]{
    If @exact{$e$} is a closed term then @exact|{$\elabstoclosedplus{e}{e'}{\tau}$}|
     implies that @exact{$e'$} is either a value or steps to an index error or
     steps to a term @exact{$e''$} with type @exact{$\tau$}.
  }

  @proof{
    We extend the existing proof with cases for @exact{$\unsaferef$}.
    @itemize[
      @item{
        If @sc{S-RefPass} is applied by the elaborator then @exact{$e'$} is
         the call @exact{$\unsaferef~\vectorvn~i$} and @exact{$0 \le i < n$}.
        By assumption, @exact|{$\typestoclosed{\vectorvn}{\tarray}$}| and
         @exact{$\typestoclosed{i}{\tint}$}.
        Therefore @exact{$e'$} is well-typed by the rule @sc{T-Unsafe} and steps
         to the integer value @exact{$v_i$} by @sc{E-UnsafePass}.
      }
      @item{
        If @sc{S-RefFail} is applied by the elaborator then @exact{$e'$} is
         @exact{$\indexerror$} and type soundness follows trivially.
      }
    ]
  }

The key step in both theorems was that elaboration supplied the proposition
 @exact{$0 \le i < n$}.
Both the type rules and evaluation rules depended on this premise.


@section{In Practice}

Have implemented for typed racket
- library
- with enhancements from @todo{secref}
- via expander, so typechecked afterwards

Checked in
- standard distro (includes plot,math,stats)
- pict3d
