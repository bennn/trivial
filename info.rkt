#lang info
(define collection 'use-pkg-name)
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "db-lib"
               "math-lib"
               "plot-lib"
               "rackunit-lib"
               "scribble-lib"))
(define build-deps '("scribble-lib"
                     "at-exp-lib"
                     "racket-doc"
                     "rackunit-abbrevs"
                     "rackunit-lib"))
(define compile-omit-paths '("icfp-2016"))
(define pkg-desc "Strongly-typed macros")
(define version "1")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/trivial.scrbl" () ("typed-racket"))))
