#lang at-exp racket

;; This module defines a gradual typing bibliography in
;; autobib format, suitable for use in papers written in Scribble

;; FIXME: this doesn't have all the papers from the README yet

(require racket/format 
         scriblib/autobib)

(provide (all-defined-out))

;; shortens names
(abbreviate-given-names #f)

;; ----------------------------------------

;; In a submodule so that it doesn't get exported automatically by
;; the outer module
(module util racket/base
  (require racket/format)
  (provide (all-defined-out))

  (define short? #f)
  (define-syntax define/short
    (syntax-rules ()
      [(_ i e e*) (define i (if short? e e*))]
      [(_ i e) (define i e)]))

  (define IEEE "IEEE ")
  (define ACM "ACM ")
  (define International "International ")
  (define Conference "Conference ")
  (define Workshop "Workshop ")
  (define Journal "Journal ")
  (define Symposium "Symposium ")
  (define Transactions "Transactions ")

  (define/short aosd "AOSD" (string-append International Conference "on Aspect-Oriented Software Development"))
  (define/short asplas "APLAS" (string-append "Asian " Symposium "Programming Languages and Systems"))
  (define/short cc "CC" (string-append International Conference "on Compiler Construction"))
  (define/short dls "DLS" "Dynamic Languages Symposium")
  (define/short dsl "DSL" (string-append ACM Conference "on Domain-specific languages"))
  (define/short dyla "DYLA" (string-append Workshop "on Dynamic Languages and Applications"))
  (define/short ecoop "ECOOP" (string-append "European " Conference "on Object-Oriented Programming"))
  (define/short esop "ESOP" (string-append "European " Symposium "on Programming"))
  (define/short flops "FLOPS" (string-append Symposium "Functional and Logic Programming"))
  (define/short foal "FOAL" "Foundations of Aspect-Oriented Languages")
  (define/short fool "FOOL" (~a International Workshop "on Foundations of Object-Oriented Languages"))
  (define/short fpca "FPCA" (string-append ACM International Conference "on Functional Programming Languages and Computer Architecture"))
  (define/short fse "FSE" (string-append International Symposium "on the Foundations of Software Engineering"))
  (define/short gpce "GPCE" "Generative Programming: Concepts & Experiences")
  (define/short haskell "Haskell Workshop")
  (define/short hosc "HOSC" "Higher-Order and Symbolic Programming")
  (define/short i&c "Info. & Comp." "Information and Computation")
  (define/short icalp "ICALP" (string-append International "Colloquium on Automata, Languages, and Programming"))
  (define/short icfp "ICFP" (string-append ACM International Conference "on Functional Programming"))
  (define/short iclp "ICLP" (string-append  International Conference "on Logic Programming"))
  (define/short icse "ICSE" (~a International Conference "on Software Engineering"))
  (define/short ieee-software (string-append IEEE "Software"))
  (define/short ifl "IFL" (string-append International Symposium "Functional and Logic Programming"))
  (define/short issta "ISSTA" (string-append International Symposium "on Software Testing and Analysis"))
  (define/short jfp "JFP" (string-append Journal "Functional Programming"))
  (define/short jsl "JSL" (string-append Journal "of Symbolic Logic"))
  (define/short lfp "LFP" "LISP and Functional Programming")
  (define/short lncs "LNCS" "Lecture Notes in Computer Science")
  (define/short lsc "LSC" "LISP and Symbolic Computation")
  (define/short ml-workshop "ML" (string-append Workshop "on ML"))
  (define/short oopsla "OOPSLA" (string-append ACM Conference "on Object-Oriented Programming, Systems, Languages and Applications"))
  (define/short padl "PADL" (string-append Symposium "on Practical Aspects of Declarative Languages"))
  (define/short pldi "PLDI" (string-append ACM Conference "on Programming Language Design and Implementation"))
  (define/short plpv "PLPV" (string-append ACM Workshop "Programming Languages meets Program Verification"))
  (define/short popl "POPL" (string-append ACM Symposium "on Principles of Programming Languages"))
  (define/short sac "SAC" (string-append Symposium "on Applied Computing"))
  (define/short scala "SCALA" (string-append "Workshop on Scala"))
  (define/short scheme-workshop "SFP" (string-append "Scheme and Functional Programming Workshop"))
  (define/short sigmod "SIGMOD" (string-append ACM "SIGMOD " International Conference "on Management of Data"))
  (define/short sigplan-notices "SIGPLAN Notices" (string-append ACM "SIGPLAN Notices"))
  (define/short tacs (string-append International Symposium "Theoretical Aspects of Computer Science"))
  (define/short tacas (string-append International Conference "on Tools and Algorithms for the Construction and Analysis of Systems"))
  (define/short tcs "Theoretical Computer Science")
  (define/short tfp "TFP" (string-append Symposium "Trends in Functional Programming"))
  (define/short tlca "TLCA" (string-append International Conference "Typed Lambda Calculi and Applications"))
  (define/short toplas "TOPLAS" (string-append Transactions "on Programming Languages and Systems"))
)
(require 'util)

;;; -- added by jan
(define bat-ecoop-2014
  (make-bib
    #:title "Understanding TypeScript"
    #:author (authors "Gavin Bierman" "Martin Abadi" "Mads Torgersen")
    #:location (proceedings-location ecoop #:pages '(257 281))
    ;#:url "http://dx.doi.org/10.1007/978-3-662-44202-9_11"
    #:date 2014))


;; ----------------------------------------
;; The original papers

(define st-sfp-2006
  (make-bib
   #:title "Gradual Typing for Functional Languages"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location (proceedings-location scheme-workshop)
   #:date 2006))

(define thf-dls-2006
  (make-bib
   #:title "Interlanguage Migration: from Scripts to Programs"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location dls #:pages '(964 974))
   #:date 2006))

(define mf-toplas-2007
  (make-bib
   #:title "Operational Semantics for Multi-Language Programs"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:date 2009
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '("12:1" "12:44"))))

(define gktff-sfp-2006
  (make-bib
   #:title "Sage: Hybrid Checking for Flexible Specifications"
   #:author (authors "Jessica Gronski" "Kenneth Knowles" "Aaron Tomb"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:date 2006
   #:location (proceedings-location scheme-workshop
                                    #:pages '(93 104))))

;; ----------------------------------------
;; Subsequent work

(define ktgff-tech-2007
  (make-bib
   #:title @~a{Sage: Unified Hybrid Checking for First-Class Types,
               General Refinement Types, and Dynamic (Extended Report)}
   #:author (authors "Kenneth Knowles" "Aaron Tomb" "Jessica Gronski"
                     "Stephen N. Freund" "Cormac Flanagan")
   #:date 2007))

(define htf-tfp-2007
  (make-bib
   #:title "Space Efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date "2007"))

(define st-ecoop-2007
  (make-bib
   #:title "Gradual Typing for Objects"
   #:author (authors "Jeremy G. Siek" "Walid Taha")
   #:location (proceedings-location ecoop #:pages '(2 27))
   #:date 2007))

(define cthf-sfp-2007
  (make-bib
   #:title "Advanced Macrology and the Implementation of Typed Scheme"
   #:author (authors "Ryan Culpepper" "Sam Tobin-Hochstadt" "Matthew Flatt")
   #:location (proceedings-location scheme-workshop #:pages '(1 13))
   #:date 2007))

(define wf-sfp-2007
  (make-bib
   #:title "Well-typed Programs Can't be Blamed"
   #:author (authors "Philip Wadler" "Robert Bruce Findler")
   #:location (proceedings-location scheme-workshop)
   #:date 2007))

(define hansen-tech-2007
  (make-bib
   #:title "Evolutionary Programming and Gradual Typing in ECMAScript 4"
   #:author "Lars T. Hansen"
   #:date 2007))

(define hf-ml-2007
  (make-bib
   #:title "Status report: specifying JavaScript with ML"
   #:author (authors "David Herman" "Cormac Flanagan")
   #:location (proceedings-location ml-workshop)
   #:date 2007))

(define thf-popl-2008
  (make-bib
   #:title "The Design and Implementation of Typed Scheme"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date 2008))

(define gray-ecoop-2008
  (make-bib
   #:title "Safe Cross-Language Inheritance"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location ecoop #:pages '(52 75))
   #:date 2008))

(define sv-dls-2008
  (make-bib
   #:title "Gradual Typing with Unification-based Inference"
   #:author (authors "Jeremy G. Siek" "Manish Vachharajani")
   #:location (proceedings-location dls)
   #:date 2008))

(define wf-esop-2009
  (make-bib
   #:title "Well-typed Programs Can't be Blamed"
   #:author (authors "Philip Wadler" "Robert Bruce Findler")
   #:location (proceedings-location esop #:pages '(1 15))
   #:date 2009))

(define sgt-esop-2009
  (make-bib
   #:title "Exploring the Design Space of Higher-Order Casts"
   #:author (authors "Jeremy G. Siek" "Ronald Garcia" "Walid Taha")
   #:location (proceedings-location esop #:pages '(17 31))
   #:date 2009))

(define gray-chapter-2009
  (make-bib
   #:title "A Model of Java/Scheme Interoperability"
   #:author "Kathryn E. Gray"
   #:date 2009))

(define mf-toplas-2009
  (make-bib
   #:title "Operational Semantics for Multi-language Programs"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '(1 44))
   #:date 2009))

(define ii-cs-2009
  (make-bib
   #:title "Gradual Typing for Featherweight Java"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (journal-location "Computer Software"
                                #:volume 26
                                #:number 2
                                #:pages '(18 40))
   #:date 2009))

(define shb-icfp-2009
  (make-bib
   #:title "A Theory of Typed Coercions and its Applications"
   #:author (authors "Nikhil Swamy" "Michael Hicks" "Gavin M. Bierman")
   #:location (proceedings-location icfp #:pages '(329 340))
   #:date 2009))

(define bfnorsvw-oopsla-2009
  (make-bib
   #:title "Thorn: Robust, Concurrent, Extensible Scripting on the JVM"
   #:author (authors "Bard Bloom" "John Field" "Nathaniel Nystrom"
                     "Johan Östlund" "Gregor Richards" "Rok Strniša"
                     "Jan Vitek" "Tobias Wrigstad")
   #:location (proceedings-location oopsla #:pages '(117 136))
   #:date 2009))

(define furr-dissertation-2009
  (make-bib
   #:title "Combining Static and Dynamic Typing in Ruby"
   #:author "Michael Furr"
   #:location (dissertation-location #:institution "University of Maryland"
                                     #:degree "Ph.D.")
   #:date 2009))

(define tobin-hochstadt-dissertation-2010
  (make-bib
   #:title "Typed Scheme: From Scripts to Programs"
   #:author "Sam Tobin-Hochstadt"
   #:location (dissertation-location #:institution "Northeastern University"
                                     #:degree "Ph.D.")
   #:date 2010))

(define wnlov-popl-2010
  (make-bib
   #:title "Integrating Typed and Untyped Code in a Scripting Language."
   #:author (authors "Tobias Wrigstad" "Francesco Zappa Nardelli"
                     "Sylvain Lebresne" "Johan Östlund" "Jan Vitek")
   #:location (proceedings-location popl #:pages '(377 388))
   #:date 2010))

(define sw-popl-2010
  (make-bib
   #:title "Threesomes, with and without blame"
   #:author (authors "Jeremy G. Siek" "Philip Wadler")
   #:location (proceedings-location popl #:pages '(365 376))
   #:date 2010))

(define htf-hosc-2010
  (make-bib
   #:title "Space-efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (journal-location hosc
                                #:volume 23
                                #:number 2
                                #:pages '(167 189))
   #:date 2010))

(define bmt-ecoop-2010
  (make-bib
   #:title "Adding Dynamic Types to C#"
   #:author (authors "Gavin Bierman" "Erik Meijer" "Mads Torgersen")
   #:location (proceedings-location ecoop #:pages '(76 100))
   #:date 2010))

(define gray-fool-2010
  (make-bib
   #:title "Interoperability in a Scripted World: Putting Inheritance and Prototypes Together"
   #:author "Kathryn E. Gray"
   #:location (proceedings-location fool)
   #:date 2010))

(define afsw-popl-2011
  (make-bib
   #:author (authors "Amal Ahmed" "Robert Bruce Findler"
                     "Jeremy G. Siek" "Philip Wadler")
   #:title "Blame for All"
   #:location (proceedings-location popl #:pages '(201 214))
   #:date 2011))

(define thscff-pldi-2011
  (make-bib
   #:title "Languages as Libraries"
   #:author (authors "Sam Tobin-Hochstadt" "Vincent St-Amour"
                     "Ryan Culpepper" "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(132 141))
   #:date 2011))

(define bce-icse-2011
  (make-bib
   #:title "Always-available Static and Dynamic Feedback"
   #:author (authors "Michael Bayne" "Richard Cook" "Michael D. Ernst")
   #:location (proceedings-location icse #:pages '(521 530))
   #:date 2011))

(define wgta-ecoop-2011
  (make-bib
   #:title "Gradual Typestate"
   #:author (authors "Roger Wolff" "Ronald Garcia"
                     "Éric Tanter" "Jonathan Aldritch")
   #:location (proceedings-location ecoop #:pages '(459 483))
   #:date 2011))

(define ii-oopsla-2011
  (make-bib
   #:title "Gradual Typing for Generics"
   #:author (authors "Lintaro Ina" "Atsushi Igarashi")
   #:location (proceedings-location oopsla #:pages '(609 624))
   #:date 2011))

(define cmscgbwf-dls-2011
  (make-bib
   #:title "The Impact of Optional Type Information on JIT Compilation of Dynamically Typed Languages"
   #:author (authors "Mason Chang" "Bernd Mathiske"
                     "Edwin Smith" "Avik Chaudhuri"
                     "Andreas Gal" "Michael Bebenita"
                     "Christian Wimmer" "Michael Franz")
   #:location (proceedings-location dls #:pages '(13 24))
   #:date 2011))

(define rch-popl-2012
  (make-bib
   #:title "The Ins and Outs of Gradual Type Inference"
   #:author (authors "Aseem Rastogi" "Avik Chaudhuri" "Basil Hosmer")
   #:location (proceedings-location popl #:pages '(481 494))
   #:date 2012))

(define dthf-esop-2012
  (make-bib
   #:title "Complete Monitors for Behavioral Contracts"
   #:author (authors "Christos Dimoulas" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(214 233))
   #:date 2012))

(define sthff-oopsla-2012
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

(define tsdthf-oopsla-2012
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland"
                     "Christos Dimoulas" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Gradual Typing for First-Class Classes"
   #:location (proceedings-location oopsla #:pages '(793 810))
   #:date 2012))

(define tsth-esop-2013
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (proceedings-location esop #:pages '(229 248))
   #:date "2013"))

(define acftd-scp-2013
  (make-bib
   #:author (authors "Esteban Allende" "Oscar Callaú" "Johan Fabry" "Éric Tanter" "Marcus Denker")
   #:title "Gradual typing for Smalltalk"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 96
                                #:number 1
                                #:pages '(52 69))
   #:date 2013))

(define aft-dls-2013
  (make-bib
   #:author (authors "Esteban Allende" "Johan Fabry" "Éric Tanter")
   #:title "Cast Insertion Strategies for Gradually-Typed Objects"
   #:location (proceedings-location dls #:pages '(27 36))
   #:date 2013))

(define vksb-dls-2014
  (make-bib
   #:author (authors "Michael M. Vitousek" "Andrew Kent" "Jeremy G. Siek" "Jim Baker")
   #:title "Design and Evaluation of Gradual Typing for Python"
   #:location (proceedings-location dls #:pages '(45 56))
   #:date 2014))

(define afgt-oopsla-2014
  (make-bib
   #:author (authors "Esteban Allende" "Johan Fabry" "Ronald Garcia" "Éric Tanter")
   #:title "Confined Gradual Typing"
   #:location (proceedings-location oopsla #:pages '(251 270))
   #:date 2014))

(define rsfbv-popl-2015
  (make-bib
   #:author (authors "Aseem Rastogi" "Nikhil Swamy" "Cédric Fournet"
                     "Gavin Bierman" "Panagiotis Vekris")
   #:title "Safe & Efficient Gradual Typing for TypeScript"
   #:location (proceedings-location popl #:pages '(167 180))
   #:date 2015))

(define gc-popl-2015
  (make-bib
   #:author (authors "Ronald Garcia" "Matteo Cimini")
   #:title "Principal Type Schemes for Gradual Programs"
   #:location (proceedings-location popl #:pages '(303 315))
   #:date 2015))

(define vcgts-esop-2015
  (make-bib
   #:title "Monotonic References for Efficient Gradual Typing"
   #:location (proceedings-location esop #:pages '(432 456))
   #:date 2015
   #:author (authors "Jeremy Siek"
                     "Michael M. Vitousek"
                     "Matteo Cimmini"
                     "Sam Tobin-Hochstadt"
                     "Ronald Garcia")))

(define tfdffthf-ecoop-2015
  (make-bib
   #:author (authors "Asumu Takikawa" "Daniel Feltey"
                     "Earl Dean" "Robert Bruce Findler"
                     "Matthew Flatt" "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:title "Towards Practical Gradual Typing"
   #:location (proceedings-location ecoop #:pages '(4 27))
   #:date 2015))

(define rnv-ecoop-2015
  (make-bib
   #:author (authors "Gregor Richards" "Francesco Zappa Nardelli" "Jan Vitek")
   #:title "Concrete Types for TypeScript"
   #:location (proceedings-location ecoop #:pages '(76 100))
   #:date 2015))

;; ----------------------------------------
; Early Work on Interoperation

(define ff-icfp-2002
  (make-bib
   #:title "Contracts for Higher-Order Functions"
   #:author (authors "Robert Bruce Findler" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(48 59))
   #:date "2002"))

;; ----------------------------------------
; Related

;; ----------------------------------------
; Contracts

(define gf-tfp-2007
  (make-bib
   #:title "Unifying Hybrid Types and Contracts"
   #:author (authors "Jessica Gronski" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date 2007))

(define bgip-esop-2011
  (make-bib
   #:title "Polymorphic Contracts"
   #:author (authors "João Filipe Belo" "Michael Greenberg"
                     "Atsushi Igarashi" "Benjamin C. Pierce")
   #:location (proceedings-location esop #:pages '(18 37))
   #:date 2011))

;; ----------------------------------------
; Typing Untyped Languages

(define cartwright-icalp-1976
  (make-bib
   #:title "User-defined Data Types as an Aid to Verifying LISP Programs"
   #:author "Robert Cartwright"
   #:location (proceedings-location icalp #:pages '(228 256))
   #:date 1976))

(define suzuki-popl-1981
  (make-bib
   #:title "Inferring Types in Smalltalk"
   #:author "Norihisa Suzuki"
   #:location (proceedings-location popl #:pages '(187 199))
   #:date 1981))

(define st-popl-1984
  (make-bib
   #:title "Creating Efficient Systems for Object-Oriented Languages"
   #:author (authors "Norihisa Suzuki" "Minoru Terada")
   #:location (proceedings-location popl #:pages '(290 296))
   #:date 1984))

(define am-popl-1991
  (make-bib
   #:title "Static Type Inference in a Dynamically Typed Language"
   #:author (authors "Alexander Aiken" "Brian R. Murphy")
   #:location (proceedings-location popl #:pages '(279 290))
   #:date 1991))

(define cf-pldi-1991
  (make-bib
   #:title "Soft Typing"
   #:author (authors "Robert Cartwright" "Mike Fagan")
   #:location (proceedings-location pldi #:pages '(278 292))
   #:date 1991))

(define bg-oopsla-1993
  (make-bib
   #:title "Strongtalk: Typechecking Smalltalk in a Production Environment"
   #:author (authors "Gilad Bracha" "David Griswold")
   #:location (proceedings-location oopsla #:pages '(215 230))
   #:date 1993))

(define awl-popl-1994
  (make-bib
   #:title "Soft Typing with Conditional Types"
   #:author (authors "Alexander Aiken" "Edward L. Wimmers" "T.K. Lakshman")
   #:location (proceedings-location popl #:pages '(163 173))
   #:date 1994))

(define henglein-scp-1994
  (make-bib
   #:author "Fritz Henglein"
   #:title "Dynamic Typing: Syntax and Proof Theory"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 22
                                #:number 3
                                #:pages '(197 230))
   #:date 1994))

(define hr-fpca-1995
  (make-bib
   #:author (authors "Fritz Henglein" "Jakob Rehof")
   #:title "Safe Polymorphic Type Inference for a Dynamically Typed Language: Translating Scheme to ML"
   #:location (proceedings-location fpca #:pages '(192 203))
   #:date 1995))

(define haynes-tech-1995
  (make-bib
   #:author "Christopher T. Haynes"
   #:title "Infer: a Statically-typed Dialect of Scheme"
   #:location (techrpt-location #:institution "Indiana University"
                                #:number "367")
   #:date 1995))

(define akers-dissertation-1996
  (make-bib
   #:title "Strong Static Type Checking for Functional Common Lisp"
   #:author "Robert Akers"
   #:location (dissertation-location #:institution "University of Texas")
   #:date 1996))

(define fagan-dissertation-1992
  (make-bib
   #:title "Soft Typing: An Approach to Type Checking for Dynamically Typed Languages"
   #:author "Mike Fagan"
   #:location (dissertation-location #:institution "Rice University")
   #:date 1992))

(define ffkwf-pldi-1996
  (make-bib
   #:title "Catching bugs in the web of program invariants"
   #:author (authors "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Stephanie Weirich"
                     "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(23 32))
   #:date 1996))

(define mw-icfp-1997
  (make-bib
   #:title "A Practical Subtyping System for Erlang"
   #:author (authors "Simon Marlow" "Philip Wadler")
   #:location (proceedings-location icfp #:pages '(136 149))
   #:date 1997))

(define wc-toplas-1997
  (make-bib
   #:title "A Practical Soft Type System for Scheme"
   #:author (authors "Andrew K. Wright" "Robert Cartwright")
   #:location (journal-location toplas
                                #:volume 19
                                #:number 1
                                #:pages '(87 152))
   #:date 1997))

;; ----------------------------------------
; Type Systems for Gradual Typing

(define thf-icfp-2010
  (make-bib
   #:title "Logical Types for Untyped Languages"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(117 128))
   #:date 2010))

(define sthff-padl-2012
  (make-bib
   #:title "Typing the Numeric Tower"
   #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt"
                     "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location padl #:pages '(289 303))
   #:date 2012))

(define fafh-sac-2009
  (make-bib
   #:title "Static Type Inference for Ruby"
   #:author (authors "Michael Furr" "Jong-hoon (David) An"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location sac #:pages '(1859 1866))
   #:date 2009))

(define acfh-popl-2011
  (make-bib
   #:title "Dynamic Inference of Static Types for Ruby"
   #:author (authors "Jong-hoon (David) An" "Avik Chaudhuri"
                     "Jeffrey S. Foster" "Michael Hicks")
   #:location (proceedings-location popl #:pages '(459 472))
   #:date 2011))

(define chj-oopsla-2012
  (make-bib
   #:title "Dependent Types for JavaScript"
   #:author (authors "Ravi Chugh" "David Herman"
                     "Ranjit Jhala")
   #:location (proceedings-location oopsla #:pages '(587 606))
   #:date 2012))

(define bonnaire-sergeant-thesis-2012
  (make-bib
   #:author "Ambrose Bonnaire-Sergeant"
   #:title "A Practical Optional Type System for Clojure"
   #:location (dissertation-location
               #:institution "University of Western Australia"
               #:degree "Honour's")
   #:date 2012))

(define mmi-dyla-2014
  (make-bib
   #:title "Typed Lua: An Optional Type System for Lua"
   #:author (authors "André Murbach Maidl" "Fabio Mascarenhas" "Roberto Ierusalimschy")
   #:location (proceedings-location dyla #:pages '(1 10))
   #:date 2014))

(define clojure-macros
  (make-bib
   #:date 2016
   #:author "Clojure 1.8.0"
   #:title "Macros"
   #:url "http://clojure.org/reference/macros"))

(define rust-compiler-plugins
  (make-bib
   #:date 2016
   #:author "Rust 1.7"
   #:title "Compiler Plugins"
   #:url "https://doc.rust-lang.org/book/compiler-plugins.html"))

(define gal-firefox
  (make-bib
   #:date 2010
   #:author "Andreas Gal"
   #:title "Proxies in Tracemonkey"
   #:url "http://hg.mozilla.org/tracemonkey/"))


(define Eiffel
  (make-bib
   #:author "Bertrand Meyer"
   #:title "Eiffel : The Language"
   #:is-book? #t
   #:date "1991"
   #:location (book-location #:publisher "Prentice Hall PTR")))

(define AmbientTalk
  (make-bib
   #:title "Ambient-Oriented Programming"
   #:author (authors "Jessie Dedecker"
                     (author-name "Tom" "Van Cutsem")
                     "Stijn Mostinckx"
                     "Theo D'Hondt"
                     (author-name "Wolfgang" "De Meuter"))
   #:date "2005"
   #:location (proceedings-location oopsla
                                    #:pages '(31 40))))

(define Euclid
  (make-bib
   #:author (authors "B. W. Lampson"
                     "J. J. Horning"
                     "R. L. London"
                     "J. G. Mitchell"
                     "G. J. Popek")
   #:title "Report on the programming language Euclid"
   #:location (journal-location sigplan-notices
                                #:volume 12
                                #:number 2
                                #:pages '(1 79))
   #:date "1977"))


(define Anna
  (make-bib
   #:author (authors "D. C. Luckham"
                     "F. W. von Henke")
   #:title "An overview of Anna, a specification language for Ada"
   #:location (journal-location ieee-software
                                #:volume 2
                                #:number 2
                                #:pages '(9 22))
   #:date "1985"))


(define D
  (make-bib
   #:author (org-author-name "Digital Mars")
   #:date "1999"
   #:title "D Programming Language"
   #:url "http://www.digitalmars.com/d/"))


(define |Operational Semantics for Multi-Language Programs|
  (make-bib
   #:title "Operational Semantics for Multi-Language Programs"
   #:author (authors "Jacob Matthews"
                     "Robert Bruce Findler")
   #:date "2009"
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '("12:1" "12:44"))))

(define TypeDynamic
  (make-bib
    #:title "Dynamic Typing in a Statically Typed Language"
    #:author (authors
	       "Martin Abadi"
	       "Luca Cardelli"
	       "Benjamin C. Pierce"
	       "Gordon D. Plotkin")
    #:date 1991
    #:location (journal-location toplas
		 #:volume 13
		 #:number 2
		 #:pages '("237" "268"))))

(define |Space Efficient Gradual Typing|
  (make-bib
   #:title "Space Efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date "2007"))

(define |Nested and Dynamic Contract Boundaries|
  (make-bib
   #:title "Nested and Dynamic Contract Boundaries"
   #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
   #:location (proceedings-location ifl
                                    #:pages '(141 158))
   #:date "2009"))

 (define ClassContracts
   (make-bib
    #:title "Contracts for First-Class Classes"
    #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
    #:location (proceedings-location dls
                                     #:pages '(97 112))
    #:date "2010"))

(define vvle-tr
  (make-bib
   #:title "Virtual Values for Language Extension"
   #:author (authors "Thomas H. Austin" "Tim Disney" "Cormac Flanagan")
   #:location (proceedings-location oopsla)
   #:date "2011"))

(define ContractsCoffee
  (make-bib
   #:title "Contracts.coffee"
   #:author "Tim Disney"
   #:date "2012"
   #:url "http://disnetdev.com/contracts.coffee/"))

(define redex-book
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

(define sfp2009-kf
  (make-bib
   #:author (authors "Casey Klein" "Robert Bruce Findler")
   #:title "Randomized Testing in PLT Redex"
   #:location (proceedings-location scheme-workshop #:pages '(26 36))
   #:date "2009"))

(define poly-sealing
  (make-bib
   #:title "Parametric Polymorphism Through Run-Time Sealing, or, Theorems for Low, Low Prices!"
   #:author (authors "Jacob Matthews" "Amal Ahmed")
   #:location (proceedings-location esop #:series "LNCS 4960" #:pages '(16 31))
   #:date 2008))

(define ciao-contracts
  (make-bib
   #:author (authors "E. Mera" "P. Lopez-Garcia" "M. Hermenegildo")
   #:title "Integrating Software Testing and Run-Time Checking in an Assertion Verification Framework"
   #:location (proceedings-location iclp #:series "LNCS 5649")
   #:date 2009))

(define no-more-scapegoating
  (make-bib
   #:title "Correct Blame for Contracts: No More Scapegoating"
   #:author (authors "Christos Dimoulas" "Robert Bruce Findler" "Cormc Flanagan" "Matthias Felleisen")
   #:location (proceedings-location popl)
   #:date 2011))

(define harmless-advice
  (make-bib
   #:title "Harmless Advice"
   #:author (authors "Daniel S. Dantas" "David Walker")
   #:location (proceedings-location popl)
   #:date 2006))

(define aspect-classification
  (make-bib
   #:title "A Classification System and Analysis for Aspect-Oriented Programs"
   #:author (authors "Martin Rinard" "Alexandru Salcianu" "Suhabe Bugrara")
   #:location (proceedings-location fse)
   #:date 2004))

(define observers-and-assistants
  (make-bib
   #:author (authors "Curtis Clifton" "Gary T. Leavens")
   #:title "Observers and assistants: A proposal for modular aspect-oriented reasoning"
   #:location (proceedings-location foal)
   #:date 2002))

(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-1")
   #:url      "http://racket-lang.org/tr1/"))

(define plt-tr3
  (make-bib
   #:title "GUI: Racket Graphics Toolkit"
   #:author (authors "Matthew Flatt" "Robert Bruce Findler" "John Clements")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-3")
   #:url      "http://racket-lang.org/tr3/"))

(define EffectiveAdvice
  (make-bib
   #:title "EffectiveAdvice: Disciplined Advice with Explicit Effects"
   #:author (authors (author-name "Bruno C. d. S." "Oliveira")
                     "Tom Schrijvers"
                     "William R. Cook")
   #:date 2010
   #:location (proceedings-location aosd)))

(define OpenModules
  (make-bib
   #:title "Open Modules: Modular Reasoning About Advice"
   #:author "Jonathan Aldrich"
   #:date 2005
   #:location (proceedings-location ecoop)))

(define MillerPhD
  (make-bib
   #:title "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control"
   #:author "M. S. Miller"
   #:is-book? #t
   #:location (dissertation-location #:institution "John Hopkins University")
   #:date 2006))

(define js-type-inference
  (make-bib #:title "Fast and precise type inference for JavaScript"
            #:author (authors (author-name "Brian" "Hackett")
                              (author-name "Shu-Yu" "Guo"))
            #:location (proceedings-location pldi) ; TODO pages
            #:date "2012"))

(define min-cover-salient-curves
  (make-bib
   #:author (authors "Pedro Felzenszwalb" "David McAllester")
   #:title "A min-cover approach for finding salient curves"
   #:location (proceedings-location (string-append IEEE Workshop "Perceptual Organization in Computer Vision"))
   #:date 2006))

(define type-soundness
  (make-bib
   #:author (authors "Andrew K. Wright" "Matthias Felleisen")
   #:title "A Syntactic Approach to Type Soundness"
   #:location (journal-location i&c #:pages '(38 94))
   #:date 1994))

(define expressive-power
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "On the Expressive Power of Programming Languages"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 17
                                #:number "1--3"
                                #:pages '(35 75))
   #:date 1991))

;; ----------------------------------------
; Software engineering

(define crtr-empirical-2013
  (make-bib
   #:author (authors "Oscar Callaú" "Romain Robbes"
                     "Éric Tanter" "David Röthlisberger")
   #:title "How (and Why) Developers Use the Dynamic Features of Programming Languages: the Case of Smalltalk"
   #:location (journal-location "Empirical Software Engineering"
                                #:volume 18
                                #:number 6
                                #:pages '(1156 1194))
   #:date 2013))

;; ----------------------------------------
; Misc

(define cm-tech-1985
  (make-bib
   #:author (authors "Robert L. Constable" "Nax P. Mendler")
   #:title "Recursive Definitions in Type Theory"
   #:location (techrpt-location #:institution "Cornell University"
                                #:number "TR 85-659")
   #:date 1985))

(define mff-popl-2006
  (make-bib
   #:author (authors "Phillipe Meunier" "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Modular Set-Based Analysis from Contracts"
   #:location (proceedings-location popl #:pages '(218 231))
   #:date 2006))

;; ----------------------------------------
; Software engineering and types

(define hkrts-empirical-2013
  (make-bib
   #:author (authors "Stefan Hanenberg" "Sebastian Kleinschmager"
                     "Romain Robbes" "Éric Tanter" "Andreas Stefik")
   #:title "An Empirical Study on the Impact of Static Typing on Software Maintainability"
   #:location (journal-location "Empirical Software Engineering"
                                ;; there really isn't any volume/number listed on Springer
                                #:pages '(1 48))
   #:date 2013))

;; ----------------------------------------
; Objects, theory

(define ac-book-1996
  (make-bib
   #:author (authors "Martin Abadi" "Luca Cardelli")
   #:title "A Theory of Objects"
   #:date 1996
   #:location (book-location #:publisher "Springer-Verlag")))

;; ----------------------------------------
; Objects, real languages

(define remy-tacs-1994
  (make-bib
    #:author "Didier Rémy"
    #:title "Programming Objects with ML-ART an Extension to ML with Abstract and Record Types"
    #:date 1994
    #:location (proceedings-location tacs #:pages '(321 346))))

(define oacddemmmsssz-tech-2006
  (make-bib
   #:title "An Overview of the Scala Programming Language"
   #:author (authors "Martin Odersky" "Philippe Altherr"
                     "Vincent Cremet" "Iulian Dragos"
                     "Gilles Dubochet" "Burak Emir"
                     "Sean McDirmid" "Stéphane Micheloud"
                     "Nikolay Mihaylov" "Michel Schinz"
                     "Erik Stenman" "Lex Spoon"
                     "Matthias Zenger")
   #:date 2006
   #:location (techrpt-location #:institution "École Polytechnique Fédérale de Lausanne"
                                #:number "LAMP-REPORT-2006-001")))

(define mme-gpce-2013
  (make-bib
   #:title "Template Constructors for Resuable Object Initialization"
   #:author (authors "Marko Martin" "Mira Mezini" "Sebastian Erdweg")
   #:location (proceedings-location gpce #:pages '(43 52))
   #:date 2013))

;; ----------------------------------------
; Macrology

(define ts-tcs-2000
  (make-bib
   #:title "MetaML and Multi-stage Programming with Explicit Annotations"
   #:author (authors "Walid Taha" "Tim Sheard")
   #:location (journal-location tcs
                                #:volume 248
                                #:number "1-2"
                                #:pages '(211 242))
   #:date 2000))

(define spj-haskell-2002
  (make-bib
   #:title "Template Meta-programming for Haskell"
   #:author (authors "Tim Sheard" "Simon Peyton Jones")
   #:location (proceedings-location haskell)
   #:date 2002))

(define burmako-scala-2013
  (make-bib
   #:title "Scala Macros: Let Our Powers Combine!"
   #:author "Eugene Burmako"
   #:location (proceedings-location "Scala Workshop")
   #:date 2013))

;; ----------------------------------------
; Contracts

(define complete-monitors
  (make-bib
   #:author (authors "Christos Dimoulas" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:title "Complete Monitors for Behavioral Contracts"
   #:location (proceedings-location esop #:pages '(214 233))
   #:date 2012))

(define ho-contract-satisfaction
  (make-bib
   #:author (authors "Christos Dimoulas" "Matthias Felleisen")
   #:title "On Contract Satisfaction in a Higher-Order World"
   #:location (journal-location toplas
                                #:volume 33
                                #:number 5
                                #:pages '("16:1" "16:29"))
   #:date 2011))

(define dimoulas-diss
  (make-bib
   #:author "Christos Dimoulas"
   #:title "Foundations for Behavioral Higher-Order Contracts"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2012))

(define nthvh-icfp-2014
  (make-bib
   #:author (authors "Phúc C. Nguyễn" "Sam Tobin-Hochstadt" "David Van Horn")
   #:title "Soft Contract Verification"
   #:location (proceedings-location icfp #:pages '(139 152))
   #:date 2014))

;; ----------------------------------------
; Proxies

(define chaperones-impersonators
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

;; ----------------------------------------
; Continuations

(define ConstrainingControl
  (make-bib
   #:author (authors "Daniel P. Friedman" "Christopher T. Haynes")
   #:title "Constraining Control"
   #:location (proceedings-location popl #:pages '(245 254))
   #:date 1985))

(define ContinuationMultiprocessing
  (make-bib
   #:author (authors "Mitchell Wand")
   #:title "Continuation-Based Multiprocessing"
   #:location (journal-location hosc
                                #:volume 12
                                #:number 3
                                #:pages '(285 299))
   #:date 1999))

(define Engines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman")
   #:title "Engines Build Process Abstractions"
   #:location (proceedings-location lfp #:pages '(18 24))
   #:date 1984))

(define Coroutines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman" "Mitchell Wand")
   #:title "Continuations and Coroutines"
   #:location (proceedings-location lfp #:pages '(293 298))
   #:date 1984))

(define GuilePrompts
  (make-bib
   #:author "Free Software Foundation"
   #:date 2012
   #:title "Guile Reference Manual: Prompts"
   #:url "http://www.gnu.org/software/guile/manual/html_node/Prompts.html"))

;; ----------------------------------------
; Continuation marks / dynamic binding

(define algebraic-stepper
  (make-bib
   #:author (authors "John Clements" "Matthew Flatt" "Matthias Felleisen")
   #:title "Modeling an Algebraic Stepper"
   #:location (proceedings-location esop #:pages '(320 334))
   #:date 2001))

(define DDBinding
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan" "Amr Sabry")
   #:title "Delimited Dynamic Binding"
   #:location (proceedings-location icfp #:pages '(26 37))
   #:date 2006))

(define GenStackInspection
  (make-bib
   #:author (authors "Greg Pettyjohn" "John Clements" "Joe Marshall"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Continuations from Generalized Stack Inspection"
   #:location (proceedings-location icfp #:pages '(216 227))
   #:date 2005))

(define AOPinHOLs
  (make-bib
   #:author (authors "David B. Tucker" "Shriram Krishnamurthi")
   #:title "Pointcuts and Advice in Higher-Order Languages"
   #:location (proceedings-location aosd #:pages '(158 167))
   #:date 2003))

(define CMsinJS
  (make-bib
   #:author (authors "John Clements" "Ayswarya Sundaram" "David Herman")
   #:title "Implementing Continuation Marks in Javascript"
   #:location (proceedings-location scheme-workshop)
   #:date 2008))

(define clements-diss
  (make-bib
   #:title "Portable and High-level Access to the Stack with Continuation Marks"
   #:author "John Clements"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2006))

;; ----------------------------------------
; Delim control

(define Felleisen88
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "The Theory and Practice of First-Class Prompts"
   #:location (proceedings-location popl #:pages '(180 190))
   #:date 1988))

(define Sitaram1993
  (make-bib
   #:author (authors "Dorai Sitaram")
   #:title "Handling Control"
   #:location (proceedings-location pldi #:pages '(147 155))
   #:date 1993))

(define DelimCompControl
  (make-bib
   #:author (authors "Matthew Flatt" "Gang Yu"
                     "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Adding Delimited and Composable Control to a Production Programming Environment"
   #:location (proceedings-location icfp #:pages '(165 176))
   #:date 2007))

(define Subcontinuations
  (make-bib
   #:author (authors "Robert Hieb" "R. Kent Dybvig" "Claude W. Anderson")
   #:title "Subcontinuations"
   #:location (journal-location lsc #:pages '(83 110))
   #:date 1994))

(define DelimiterHierarchies
  (make-bib
   #:author (authors "Dorai Sitaram" "Matthias Felleisen")
   #:title "Control Delimiters and their Hierarchies"
   #:location (journal-location lsc #:pages '(67 99))
   #:date 1990))

(define MachIV
  (make-bib
   #:author "Richard P. Draves"
   #:title "Control Transfer in Operating System Kernels"
   #:is-book? #t
   #:location (dissertation-location #:institution "Carnegie Mellon University")
   #:date 1994))

;; ----------------------------------------
; Types for untyped languages

(define rtsf-oops-2013
  (make-bib
   #:author (authors "Brianna M. Ren" "John Toman" "T. Stephen Strickland" "Jeffrey S. Foster")
   #:title "The Ruby Type Checker"
   #:location (proceedings-location sac #:pages '(1565 1572))
   #:date 2013))

;; ----------------------------------------
; Gradual typing

;; for these papers, see
;; https://github.com/samth/gradual-typing-bib/blob/master/main.rkt

(define typescript
  (make-bib
   #:title "Typescript Language Specification"
   #:location (techrpt-location #:number "Version 0.9.1"
                                #:institution "Microsoft")
   #:date 2013))

;; ----------------------------------------
; Components and modules

(define mfh-oopsla-2001
  (make-bib
   #:title "Jiazzi: New-Age Components for Old-Fashioned Java"
   #:author (authors "Sean McDirmid" "Matthew Flatt"
                     "Wilson C. Hsleh")
   #:date 2001
   #:location (proceedings-location oopsla #:pages '(211 222))))

(define fg-ml-2010
  (make-bib
   #:title "First-class Modules and Composable Signatures in Objective Caml 3.12"
   #:author (authors "Alain Frisch" "Jacques Garrique")
   #:date 2010
   #:location (proceedings-location ml-workshop)))

;; ----------------------------------------
; Mixins and traits

(define fkf-popl-1998
  (make-bib
   #:title "Classes and Mixins"
   #:author (authors "Matthew Flatt" "Shriram Krishnamurthi"
                     "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(171 183))
   #:date 1998))

(define alz-ecoop-2000
  (make-bib
   #:title "Jam - A Smooth Extension of Java with Mixins"
   #:author (authors "Davide Ancona" "Giovanni Lagorio" "Elena Zucca")
   #:location (proceedings-location esop #:pages '(154 178))
   #:date 2000))

(define abc-oopsla-2003
  (make-bib
   #:title "A First-class Approach to Genericity"
   #:author (authors "Eric Allen" "Jonathan Bannet"
                     "Robert Cartwright")
   #:location (proceedings-location oopsla #:pages '(96 114))
   #:date 2003))

(define sdnb-ecoop-2003
  (make-bib
   #:title "Traits: Composable Units of Behaviour"
   #:author (authors "Nathanael Schärli" "Stéphane Ducasse"
                     "Oscar Nierstrasz" "Andrew P. Black")
   #:location (proceedings-location ecoop #:pages '(248 274))
   #:date 2003))

(define kt-asplas-2004
  (make-bib
   #:title "McJava – A Design and Implementation of Java with Mixin-Types"
   #:author (authors "Tetsuo Kamina" "Tetsuo Tamai")
   #:location (proceedings-location asplas #:pages '(398 414))
   #:date 2004))

(define sd-ecoop-2005
  (make-bib
   #:title "Chai: Traits for Java-Like Languages"
   #:author (authors "Charles Smith" "Sophia Drossopoulou")
   #:location (proceedings-location ecoop #:pages '(453 478))
   #:date 2005))

(define sz-oopsla-2010
  (make-bib
   #:title "MetaFJig: a Meta-circular Composition Language for Java-like Classes"
   #:author (authors "Marco Servetto" "Elena Zucca")
   #:location (proceedings-location oopsla #:pages '(464 483))
   #:date 2010))

;; ----------------------------------------
; Beta and Beta-style programming

(define mmpn-book-1993
  (make-bib
   #:title "Object-Oriented Programming in the BETA Programming Language"
   #:author (authors "Ole Lehrmann Madsen" "Birger Møller-Pedersen"
                     "Kristen Nygaard")
   #:date 1993
   #:location (book-location #:publisher "Addison-Wesley Publishing Co.")))

(define gff-oopsla-2004
  (make-bib
   #:title "Super and Inner: Together at Last!"
   #:author (authors "David S. Goldberg" "Robert Bruce Findler"
                     "Matthew Flatt")
   #:date 2004
   #:location (proceedings-location oopsla #:pages '(116 129))))

;; ----------------------------------------
; Types for delim control

(define Danvy1989
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "A Functional Abstraction of Typed Contexts"
   #:location (techrpt-location #:institution "University of Copenhagen"
                                ;; I'm not 100% sure of the TR number since
                                ;; it's not listed anywhere officially
				#:number "DIKU Report 89/12")
   #:date 1989))

(define AbstractingControl
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "Abstracting Control"
   #:location (proceedings-location lfp #:pages '(151 160))
   #:date 1990))

(define Gunter1995
  (make-bib
    #:author (authors "Carl A. Gunter" "Remy Didier" "Jon G. Riecke")
    #:title "A Generalization of Exceptions and Control in ML-like Languages"
    #:location (proceedings-location fpca #:pages '(12 23))
    #:date 1995))

(define Kiselyov2007
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan")
   #:title "A Substructural Type System for Delimited Continuations"
   #:location (proceedings-location tlca #:pages '(223 239))
   #:date 2007))

(define Asai2007
  (make-bib
    #:author (authors "Kenichi Asai" "Yukiyoshi Kameyama")
    #:title "Polymorphic Delimited Continuations"
    #:location (proceedings-location asplas #:pages '(239 254) #;#:series #;"LNCS 4807")
    #:date 2007))

(define Dybvig2007
  (make-bib
   #:author (authors "Kent Dybvig"
                     "Simon Peyton-Jones"
                     "Amr Sabry")
   #:title "A Monadic Framework for Delimited Continuations"
   #:location (journal-location jfp
                                #:volume 17
                                #:number 6
                                #:pages '(687 730))
   #:date 2007))

(define James2011
  (make-bib
   #:author (authors "Roshan P. James" "Amr Sabry")
   #:title "Yield: Mainstream Delimited Continuations"
   #:location (proceedings-location "Theory and Practice of Delimited Continuations" #:pages '(20 32))
   #:date 2011))

(define control-tr
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (techrpt-location #:institution "Northeastern University"
				#:number "NU-CCIS-13-01")
   #:date "2013"))

;; ----------------------------------------
; Education

(define scriven-chapter-1967
  (make-bib
   #:author "Michael Scriven"
   #:title "The Methodology of Evaluation. Perspectives of Curriculum Evaluation"
   #:location (book-location #:publisher "Rand McNally")
   #:date 1967))

(define fcffksf-jfp-2002
  (make-bib
   #:author (authors "Robert Bruce Findler" "John Clements"
                     "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Paul Steckler"
                     "Matthias Felleisen")
   #:title "DrScheme: a Programming Environment for Scheme"
   #:location (journal-location jfp
                                #:volume 12
                                #:number 2
                                #:pages '(159 182))
   #:date 2002))

(define fffk-icfp-2009
  (make-bib
   #:author (authors "Matthias Felleisen" "Robert Bruce Findler"
                     "Matthew Flatt" "Shriram Krishnamurthi")
   #:title "A Functional I/O System (or Fun for Freshman Kids)"
   #:location (proceedings-location icfp #:pages '(47 58))
   #:date 2009))

;; ----------------------------------------
; Racket

(define ffkf-icfp-1999
  (make-bib
   #:author (authors "Matthew Flatt" "Rober Bruce Findler"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Programming Languages as Operating Systems (or Revenge of the Son of the Lisp Machine)"
   #:location (proceedings-location icfp #:pages '(138 147))
   #:date 1999))

(define fff-asplas-2006
  (make-bib
   #:author (authors "Matthew Flatt" "Robert Bruce Findler"
                     "Matthias Felleisen")
   #:title "Scheme with Classes, Mixins, and Traits"
   #:location (proceedings-location asplas #:pages '(270 289))
   #:date 2006))

(define fbf-icfp-2009
  (make-bib
   #:author (authors "Matthew Flatt" "Eli Barzilay"
                     "Robert Bruce Findler")
   #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
   #:location (proceedings-location icfp #:pages '(109 120))
   #:date 2009))

(define st-icfp-2013
  (make-bib
   #:author (authors "Vincent St-Amour" "Neil Toronto")
   #:title "Applying Random Testing to a Base Type Environment"
   #:location (proceedings-location icfp #:pages '(351 356))
   #:date 2013))

(define saf-cc-2015
  (make-bib
   #:author (authors "Vincent St-Amour" "Leif Andersen" "Matthias Felleisen")
   #:title "Feature-specific Profiling"
   #:location (proceedings-location cc #:pages '(49 68))
   #:date 2015))

(define stf-optimization-coaching
  (make-bib
    #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt" "Matthias Felleisen")
    #:title "Optimization coaching"
    #:location (proceedings-location oopsla #:pages '(163 178))
    #:date 2012))

;; ----------------------------------------
; Pycket

(define fbpsth-dyla-2014
  (make-bib
   #:author (authors "Carl Friedrich Bolz" "Tobias Pape"
                     "Jeremy G. Siek" "Sam Tobin-Hochstadt")
   #:title "Meta-tracing makes a fast Racket"
   #:location (proceedings-location dyla)
   #:date 2014))

(define bauman-et-al-icfp-2015
  (make-bib
   #:author (authors "Spenser Bauman" "Carl Friedrich Bolz" "Robert Hirschfield"
                     "Vasily Kirilichev" "Tobias Pape" "Jeremy G. Siek"
                     "Sam Tobin-Hochstadt")
   #:title "Pycket: A Tracing JIT For a Functional Language"
   #:location (proceedings-location icfp #:pages '(22 34))
   #:date 2015))

;; ----------------------------------------
; Pluggable types

(define bracha-pluggable-types
  (make-bib
   #:author "Gilad Bracha"
   #:title "Pluggable Type Systems"
   #:location (proceedings-location "OOPSLA Workshop on Revival of Dynamic Languages")
   #:date 2004))

(define pacpe-issta-2008
  (make-bib
   #:author (authors "Matthew M. Papi" "Mahmood Ali" "Telmo Louis Correa, Jr."
                     "Jeff H. Perkins" "Michael D. Ernst")
   #:title "Practical Pluggable Types for Java"
   #:location (proceedings-location issta #:pages '(201 212))
   #:date 2008))

;; ----------------------------------------
; Ancient history

(define moon-maclisp-1974
  (make-bib
   #:author "David A. Moon"
   #:title "MACLISP Reference Manual"
   #:date 1974))

(define f-scp-1991
  (make-bib
   #:author "Matthias Felleisen"
   #:title "On the Expressive Power of Programming Languages"
   ;#:location (proceedings-location "Science of Computer Programming")
   #:date 1991))

(define hm-icfp-2004
  (make-bib
   #:author "David Herman and Philippe Meunier"
   #:title "Improving the Static Analysis of Embedded Languages via Partial Evaluation"
   #:location (proceedings-location icfp) ;#:pages '()
   #:date 2004))

(define fi-jfp-2000
  (make-bib
   #:title "Do we need dependent types?"
   #:author "Daniel Friedlander and Mia Indrika"
   #:location (journal-location jfp
                                #:volume 10
                                #:number 4
                                #:pages '(409 415))
   #:date 2000))

(define lb-sigplan-2014
  (make-bib
   #:title "Hasochism: The Pleasure and Pain of Dependently Typed Programming"
   #:author "Sam Lindley and Conor McBride"
   #:location (proceedings-location sigplan-notices #:pages '(81 92))
   #:date 2014))

(define ddems-icse-2011
  (make-bib
   #:title "Building and Using Pluggable Type Checkers"
   #:author (authors "W. Dietl" "S. Dietzel" "M. D. Ernst" "K. Muslu" "T. W. Schiller")
   #:location (proceedings-location icse)
   #:date 2011))

(define a-icfp-1999
  (make-bib
   #:title "Cayenne --- a language with dependent types"
   #:author "Lennart Augustsson"
   #:location (proceedings-location icfp #:pages '(239 250))
   #:date 1998))

(define f-popl-2016
  (make-bib
   #:title "Bindings as Sets of Scopes"
   #:author "Matthew Flatt"
   #:location (proceedings-location popl #:pages '(705 717))
   #:date 2016))

(define c-jsl-1997
  (make-bib
   #:title "Three Uses of the Herbrand-Gentzen theorem in relating model theory and proof theory"
   #:author "William Craig"
   #:location (journal-location jsl
                                #:volume 22
                                #:number 3
                                #:pages '(269 285))
   #:date 1957))

(define wmpk-algol-1968
  (make-bib
   #:title "Report on the Algorithmic Language ALGOL 68"
   #:author (authors "A. van Wijngaarden" "B. J. Mailloux" "J.E.L. Peck" "C.H.A. Koster")
   #:date 1968))

(define s-lisp-1990
  (make-bib
   #:title "Common Lisp the Language"
   #:author "Guy L. Steele"
   #:location (book-location #:edition "2nd" #:publisher "Digital Press")
   #:is-book? #t
   #:date 1990))

(define mbb-sigmod-2006
  (make-bib
   #:title "LINQ: Reconciling Object, Relations and XML in the .NET Framework"
   #:author (authors "Erik Meijer" "Brain Beckman" "Gavin Bierman")
   #:location (proceedings-location sigmod #:pages '(706 706))
   #:date 2006))

(define c-dissertation-2010
  (make-bib
   #:title "Refining Syntactic Sugar: Tools for Supporting Macro Development"
   #:author "Ryan Culpepper"
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2010))

(define f-icfp-2002
  (make-bib
   #:title "Composable and Compilable Macros: You Want it When?"
   #:author "Matthew Flatt"
   #:location (proceedings-location icfp #:pages '(72 83))
   #:date 2002))

(define ew-haskell-2012
  (make-bib
   #:title "Dependently Typed Programming with Singletons"
   #:author "Richard A. Eisenberg and Stephanie Weirich"
   #:location (proceedings-location haskell #:pages '(117 130))
   #:date 2012))

(define ks-plpv-2006
  (make-bib
   #:title "Lightweight static capabilities"
   #:author "Oleg Kiselyov and Chung-chieh Shan"
   #:location (proceedings-location plpv)
   #:date 2006))

(define b-scala-2013
  (make-bib
   #:title "Scala Macros: Let our Powers Combine!"
   #:author "Eugene Burmako"
   #:location (proceedings-location scala)
   #:date 2013))

(define ro-gpce-2010
  (make-bib
   #:title "Lightweight Modular Staging: A Pragmatic Approach to Runtime Code Generation and Compiled DSLs"
   #:author (authors "Tiark Rompf" "Martin Odersky")
   #:location (proceedings-location gpce)
   #:date 2010))

(define kkt-pldi-2016
  (make-bib
   #:title "Occurrence Typing Modulo Theories"
   #:author (authors "Andrew Kent" "David Kempe" "Sam Tobin-Hochstadt")
   #:location (proceedings-location pldi)
   #:date 2016))

(define fcdb-jfp-2012
  (make-bib
   #:title "Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts"
   #:author (authors "Matthew Flatt" "Ryan Culpepper" "David Darais" "Robert Bruce Findler")
   #:location (proceedings-location jfp #:pages '(181 216))
   #:date 2012))

(define ramho-hosc-2013
  (make-bib
   #:title "Scala-Virtualized: Linguistic Reuse for Deep Embeddings"
   #:author (authors "Tiark Rompf" "Nada Amin" "Adriaan Moors" "Philipp Haller" "Martin Odersky")
   #:location (journal-location hosc #:volume 25 #:number 1 #:pages '(165 207))
   #:date 2012))

(define stf-esop-2009
  (make-bib
   #:title "Practical Variable-Arity Polymorphism"
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(32 46))
   #:date 2009))

(define ra-icfp-2015
  (make-bib
   #:title "Functional Pearl: A SQL to C compiler in 500 Lines of Code"
   #:author (authors "Tiark Rompf" "Nada Amin")
   #:location (proceedings-location icfp #:pages '(2 9))
   #:date 2015))

(define lm-dsl-1999
  (make-bib
   #:title "Domain Specific Embedded Compilers"
   #:author (authors "Daan Leijen" "Erik Meijer")
   #:location (proceedings-location dsl #:pages '(109 122))
   #:date 1999))

(define cf-icfp-2010
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(235 246))
   #:date 2010))

(define pss-tacas-1998
  (make-bib
   #:title "Translation Validation"
   #:author (authors "Amir Pnueli" "Michael Siegel" "Eli Singerman")
   #:location (proceedings-location tacas #:pages '(151 166))
   #:date 1998))

(define le-popl-2016
  (make-bib
    #:title "Sound Type-Dependent Syntactic Language Extension"
    #:author (authors "Florian Lorenzen" "Sebastian Erdweg")
    #:location (proceedings-location popl #:pages '(204 216))
    #:date 2016))

(define gr-cup-2004
  (make-bib
    #:title "The Standard ML Base Library"
    #:author (authors "Emden R. Gansner" "John H. Reppy")
    #:location (book-location #:edition "1" #:publisher "Cambridge University Press")
    #:date 2004))
