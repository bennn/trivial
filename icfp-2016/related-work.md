related-work
===

All papers in `src/` directory.

Many workarounds = polydots, pi-types, printf-types, ...

Herman Meunier, 2004
---
- Better support for embedded (string) DSLs
- Scheme has an HTML DSL?
- They use identifier macros!
  Just like today except theyre "encouraging" re-implementing regexp and format



[Politz, Q-D, Krishnamurthi](http://cs.brown.edu/research/plt/dl/progressive-types/progressive-types.pdf)
---
- turning up the class of type errors
- progressively say what more should go wrong


[neelk](/Users/ben/code/racket/my-pkgs/trivial/icfp-2016/src/k-obt-2016.pdf )
---
- breakup into more steps
- elaborte from sexp to binding trees #nuprl, then to lambda terms
0 if this niggas just doing stlc that will be very sad



[oleg](http://okmij.org/ftp/Computation/lightweight-dependent-typing.html)
---
- ADTs can enforce propositions.
  If you functor apply to get an abstract set, you have a property about the integers:
    \forall n,m . n /= m -> mem? n (singleton m) == False
- singletons are for types with unique inhabitants
- Dont understand hongwei example
  - http://okmij.org/ftp/Haskell/eliminating-array-bound-check.lhs


[Lightweight Static Capabilites]
[Lightweight Static Resources]



[Singletons]
---
- Eisenberg, Weirich
- Kicking my ass a little, do printf + nary zip in parentheses; db as main thing
- library does the boilerplate of writing types/functions a second time
- 


[Hasochism]
---


[Pluggable Types]
---

Especially Ernst, Dietl and the regular-expression types for Java.
Maybe just regexp types but at least that, ya know?


QDSL / LINQ
---

- http://homepages.inf.ed.ac.uk/slindley/papers/qdsl-draft-february2015.pdf
  - host(C) + target(SQL) language (can we apply elsewhere)
  - 


- http://homepages.inf.ed.ac.uk/slindley/papers/practical-theory-of-linq.pdf
  -

- https://homes.cs.washington.edu/~akcheung/papers/cidr13.pdf
  - java Hibernate? this paper just compiles java to sql


- http://infoscience.epfl.ch/record/180642/files/EPFL_TH5456.pdf
  - generate SQL with embedded compiler
  - wow, Tiarks thesis is looking very familiar (to me)

- http://hackage.haskell.org/package/opaleye
- (dead?) https://wiki.haskell.org/MetaHDBC
  - https://lindstroem.files.wordpress.com/2008/09/metahdbc.pdf

;; TODO related work?
;; - OCaml https://github.com/mmottl/postgresql-ocaml/
;;   seems to return strings
;; - Haskell http://book.realworldhaskell.org/read/using-databases.html
;;   get 'SqlString', etc


[Futamuru](http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html)
---

Partial evaluation
- specialize interpreter for source code; given fixed input, specialize
  - can use the specializing machine as a compiler
  - (compile programs to machines)
- specialize specializer to interpreter, make compiler for class of interpreters
  - (make specific interpreter->compiler machine)
- specliaze specializer to specializer = interpreter->compiler machine
  - (make interpreter->compiler machine)


@; Tuples in Haskell http://hackage.haskell.org/package/tuple
@; Regexp
@; - ocaml http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
@; - haskell https://hackage.haskell.org/package/regex-compat/docs/Text-Regex.html



SoundX -- cannot reporduce this because      

[anti-TH](http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell)
---
Not great news for me / metaprogramming
