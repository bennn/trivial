#lang racket/base

(provide
  (rename-out
    [-make-list make-list]
    [-build-list build-list]
    [-cons cons]
    [-car car]
    [-cdr cdr]
    [-list list]
    [-length length]
    [-list-ref list-ref]
    [-list-tail list-tail]
    [-append append]
    [-reverse reverse]
    [-map map]
    [-sort sort]
    #;[-andmap andmap]
    #;[-ormap ormap]
    #;[-for-each for-each]
    #;[-foldl foldl]
    #;[-foldr foldr]
    #;[-filter filter]
    #;[-remove remove]
    #;[-remq remq]
    #;[-remv remv]
    #;[-remove* remove*]
    #;[-remq* remq*]
    #;[-remv* remv*]
    #;[-member member])
)

;; -----------------------------------------------------------------------------

(module typed-list typed/racket
  (require
    typed/racket/unsafe
    (only-in racket/unsafe/ops
      unsafe-car
      unsafe-cdr))

  ;; Thank you based Asumu
  (unsafe-require/typed racket/unsafe/ops
    (unsafe-cons-list (All (A B)
     (-> A B (Pairof A B))))
    (unsafe-list-ref (All (A B)
     (-> (Listof A) B A)))
    (unsafe-list-tail (All (A B C)
     (-> (Pairof A B) C B))))

  (unsafe-provide
    unsafe-cons-list unsafe-list-ref unsafe-list-tail)

  (provide
    unsafe-car unsafe-cdr null make-list build-list cons car cdr length list
    list-ref list-tail append reverse map sort)
)

(module untyped-list racket/base
  (require
    racket/list
    racket/unsafe/ops)
  (provide
    unsafe-cons-list unsafe-list-ref unsafe-list-tail
    unsafe-car unsafe-cdr null make-list build-list cons car cdr length list
    list-ref list-tail append reverse map sort)
)

;; -----------------------------------------------------------------------------

(require
  (prefix-in τ- 'typed-list)
  (prefix-in λ- 'untyped-list)
  trivial/private/function
  trivial/private/integer
  trivial/private/tailoring
  (for-syntax
    typed/untyped-utils
    syntax/parse
    racket/base
    racket/syntax
    trivial/private/sequence-domain
    trivial/private/common))

;; =============================================================================

(define-tailoring (-make-list [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                              [e2 ~> e2+ (φ2)]) 
  #:with +ml (τλ #'τ-make-list #'λ-make-list)
  #:= (⊥? I-dom i)
      (+ml e1+ e2+)
  #:+ #t
      (+ml e1+ e2+)
  #:φ (φ-set (φ-init) L-dom i))

(define-tailoring (-build-list [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                               [e2 ~> e2+ (φ2 [A-dom ↦ a])])
  #:with +bl (τλ  #'τ-build-list #'λ-build-list)
  (define arity-ok? (or (⊥? A-dom a) (= (length a) 1)))
  (define i-⊥? (⊥? I-dom i))
  #:= (and i-⊥? arity-ok?)
      (+bl e1+ e2+)
  #:+ (and (not i-⊥?) arity-ok?)
      (+bl e1+ e2+)
  #:- #t
      (format-arity-error #'e2+ 1)
  #:φ (φ-set (φ-init) L-dom i))

(define-tailoring (-cons [e1 ~> e1+ (φ1)]
                         [e2 ~> e2+ (φ2 [L-dom ↦ n])])
  #:with +cons (τλ #'τ-cons #'λ-cons)
  #:= (⊥? L-dom n)
      (+cons e1+ e2+)
  #:+ #t
      (+cons e1+ e2+)
  #:φ (φ-set (φ-init) L-dom (reduce L-dom + n 1)))

(define-tailoring (-list [e* ~> e+* (φ*)] ...)
  #:with +list (τλ #'τ-list #'λ-list)
  #:+ #t (+list e+* ...)
  #:φ (φ-set (φ-init) L-dom (length φ*)))

(define-tailoring -null
  #:with +null (τλ #'τ-null #'λ-null)
  #:+ #t +null
  #:φ (φ-set (φ-init) L-dom 0))

(define-tailoring (-car [e ~> e+ (φ [L-dom ↦ n])])
  #:with +car (τλ #'τ-car #'λ-car)
  #:with +unsafe-car (τλ #'τ-unsafe-car #'λ-unsafe-car)
  #:= (⊥? L-dom n)
      (+car e+)
  #:+ (< 0 n)
      (+unsafe-car e+)
  #:- #t
      (format-bounds-error #'e+ 0)
  #:φ (φ-init))

(define-tailoring (-cdr [e ~> e+ (φ [L-dom ↦ n])])
  #:with +cdr (τλ #'τ-cdr #'λ-cdr)
  #:with +unsafe-cdr (τλ #'τ-unsafe-cdr #'λ-unsafe-cdr)
  #:= (⊥? L-dom n)
      (+car e+)
  #:+ (< 0 n)
      (+unsafe-cdr e+)
  #:- #t
      (format-bounds-error #'e+ 0)
  #:φ (φ-init))

(define-tailoring (-length [e ~> e+ (φ [L-dom ↦ n])])
  #:with +length (τλ #'τ-length #'λ-length)
  #:= (⊥? L-dom n)
      (+length e+)
  #:+ #t '#,n
  #:φ (φ-set (φ-init) I-dom n))

(define-tailoring (-list-ref [e1 ~> e1+ (φ1 [L-dom ↦ n])]
                             [e2 ~> e2+ (φ2 [I-dom ↦ i])])
   #:with +list-ref (τλ #'τ-list-ref #'λ-list-ref)
   #:with +unsafe-list-ref (τλ #'τ-unsafe-list-ref #'λ-unsafe-list-ref)
   #:= (or (⊥? L-dom n) (⊥? I-dom i))
       (+list-ref e1+ e2+)
   #:+ (and (<= 0 i) (< i n))
       (+unsafe-list-ref e1+ e2+)
   #:- #t
       (format-bounds-error #'e1+ i)
   #:φ (φ-init))

(define-tailoring (-list-tail [e1 ~> e1+ (φ1 [L-dom ↦ n])]
                              [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +list-tail (τλ #'τ-list-tail #'λ-list-tail)
  #:with +unsafe-list-tail (τλ #'τ-unsafe-list-tail #'λ-unsafe-list-tail)
  #:= (or (⊥? L-dom n) (⊥? I-dom i))
      (+list-tail e1+ e2+)
  #:+ (<= 0 i n)
      (+unsafe-list-tail e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) L-dom i))

(define-tailoring (-append [e* ~> e+* (φ* [L-dom n*])] ...)
  #:with +append (τλ #'τ-append #'λ-append)
  #:= (⊥? L-dom (⊓* L-dom n*))
      (+append e+* ...)
  #:+ #t
      (+append e+* ...)
  #:φ (φ-set (φ-init) L-dom (reduce* L-dom + 0 n*)))

(define-tailoring (-reverse [e ~> e+ (φ [L-dom n])])
  #:with +reverse (τλ #'τ-reverse #'λ-reverse)
  #:= (⊥? L-dom n)
      (+reverse e+)
  #:+ #t (+reverse e+)
  #:φ (φ-set (φ-init) L-dom n))

(define-tailoring (-map [f ~> f+ (φ1 [A-dom a])]
                        [e* ~> e+* (φ* [L-dom n*])] ...)
  #:with +map (τλ #'τ-map #'λ-map)
  (define expected-arity (length n*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  #:= (and (⊥? L-dom (⊓* L-dom n*)) arity-ok?)
      (+map f+ e+* ...)
  #:+ arity-ok?
      (+map f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) L-dom (⊓* L-dom n*)))

(define-tailoring (-sort [e ~> e+ (φ [L-dom ↦ n])]
                         [e* ~> e+* (φ*)] ...)
  #:with +sort (τλ #'τ-sort #'λ-sort)
  #:= (⊥? L-dom n)
      (+sort e+ e+* ...)
  #:+ #t (+sort e+ e+* ...)
  #:φ φ)
