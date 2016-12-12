#lang info
(define collection "trivial")
(define deps '("base"
               "db-lib"
               "math-lib"
               "plot-lib"
               "rackunit-lib"
               "reprovide-lang"
               "scribble-lib"
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("at-exp-lib"
                     "racket-doc"
                     "rackunit-abbrevs"
                     "rackunit-lib"
                     "scribble-lib"))
(define pkg-desc "Macros for lightweight program analysis")
(define version "2")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/trivial.scrbl" () ("typed-racket"))))
(define raco-commands '(("trivial" (submod trivial/private/raco-command main) "Compile and log optimizations" #f)))
