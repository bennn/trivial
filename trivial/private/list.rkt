#lang racket/base

(provide
  (rename-out
    [-make-list make-list]
    [-build-list build-list]
    [-cons cons]
    [-car car]
    [-cdr cdr]
    [-first first]
    [-second second]
    [-third third]
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
    first second third
    list-ref list-tail append reverse map sort)
)

(module untyped-list racket/base
  (require
    racket/list
    racket/unsafe/ops)
  (provide
    unsafe-cons-list unsafe-list-ref unsafe-list-tail
    unsafe-car unsafe-cdr null make-list build-list cons car cdr length list
    first second third
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
    (only-in trivial/private/sequence-domain
      φ*-null? make-φ* format-bounds-error)
    (rename-in trivial/private/sequence-domain
      [list-domain L-dom]
      [vector-domain V-dom]
      [list-domain->I-dom L->I]
      [I-dom->list-domain I->L]
      [list-domain-cons L-cons]
      [list-domain-car L-car]
      [list-domain-first L-first]
      [list-domain-second L-second]
      [list-domain-third L-third]
      [list-domain-cdr L-cdr]
      [list-domain-reverse L-reverse]
      [list-domain-length L-length]
      [list-domain-ref L-ref]
      [list-domain-set L-set]
      [list-domain-append* L-append*]
      [list-domain->vector-domain L->V]
      [list-domain-slice L-slice])
    trivial/private/common))

;; =============================================================================

(define-tailoring (-make-list [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                              [e2 ~> e2+ (φ2)]) 
  #:with +ml (τλ #'τ-make-list #'λ-make-list)
  #:= (⊥? I-dom i)
      (+ml e1+ e2+)
  #:+ #t
      (+ml e1+ e2+)
  #:φ (φ-set (φ-init) L-dom (make-φ* i φ2)))

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
  #:φ (φ-set (φ-init) L-dom (I->L i)))

(define-tailoring (-cons [e1 ~> e1+ (φ1)]
                         [e2 ~> e2+ (φ2 [L-dom ↦ l])])
  #:with +cons (τλ #'τ-cons #'λ-cons)
  #:= (⊥? L-dom l)
      (+cons e1+ e2+)
  #:+ #t
      (+cons e1+ e2+)
  #:φ (φ-set (φ-init) L-dom (L-cons φ1 l)))

(define-tailoring (-list [e* ~> e+* (φ*)] ...)
  #:with +list (τλ #'τ-list #'λ-list)
  #:+ #t (+list e+* ...)
  #:φ (φ-set (φ-init) L-dom φ*))

(define-tailoring -null
  #:with +null (τλ #'τ-null #'λ-null)
  #:+ #t +null
  #:φ (φ-set (φ-init) L-dom '()))

(define-tailoring (-car [e ~> e+ (φ [L-dom ↦ l])])
  #:with +car (τλ #'τ-car #'λ-car)
  #:with +unsafe-car (τλ #'τ-unsafe-car #'λ-unsafe-car)
  #:= (⊥? L-dom l)
      (+car e+)
  #:+ (not (φ*-null? l))
      (+unsafe-car e+)
  #:- #t
      (format-bounds-error #'e+ 0)
  #:φ (L-car l))

(define-tailoring (-cdr [e ~> e+ (φ [L-dom ↦ l])])
  #:with +cdr (τλ #'τ-cdr #'λ-cdr)
  #:with +unsafe-cdr (τλ #'τ-unsafe-cdr #'λ-unsafe-cdr)
  #:= (⊥? L-dom l)
      (+cdr e+)
  #:+ (not (φ*-null? l))
      (+unsafe-cdr e+)
  #:- #t
      (format-bounds-error #'e+ 0)
  #:φ (φ-set (φ-init) L-dom (L-cdr l)))

(define-tailoring (-first [e ~> e+ (φ [L-dom ↦ l])])
  #:with +first (τλ #'τ-first #'λ-first)
  #:= (⊥? L-dom l)
      (+first e+)
  #:+ (not (φ*-null? l))
      (+first e+) ;; TODO make unsafe
  #:- #t
      (format-bounds-error #'e+ 1)
  #:φ (L-first l))

(define-tailoring (-second [e ~> e+ (φ [L-dom ↦ l])])
  #:with +second (τλ #'τ-second #'λ-second)
  #:= (⊥? L-dom l)
      (+second e+)
  #:+ (not (φ*-null? l))
      (+second e+) ;; TODO make unsafe
  #:- #t
      (format-bounds-error #'e+ 1)
  #:φ (L-second l))

(define-tailoring (-third [e ~> e+ (φ [L-dom ↦ l])])
  #:with +third (τλ #'τ-third #'λ-third)
  #:= (⊥? L-dom l)
      (+third e+)
  #:+ (not (φ*-null? l))
      (+third e+) ;; TODO make unsafe
  #:- #t
      (format-bounds-error #'e+ 2)
  #:φ (L-third l))

(define-tailoring (-length [e ~> e+ (φ [L-dom ↦ l])])
  #:with +length (τλ #'τ-length #'λ-length)
  (define n (L-length l))
  #:= (⊥? L-dom l)
      (+length e+)
  #:+ #t '#,n
  #:φ (φ-set (φ-init) I-dom n))

(define-tailoring (-list-ref [e1 ~> e1+ (φ1 [L-dom ↦ l])]
                             [e2 ~> e2+ (φ2 [I-dom ↦ i])])
   #:with +list-ref (τλ #'τ-list-ref #'λ-list-ref)
   #:with +unsafe-list-ref (τλ #'τ-unsafe-list-ref #'λ-unsafe-list-ref)
   (define n (L-length l))
   #:= (or (⊥? I-dom n) (⊥? I-dom i))
       (+list-ref e1+ e2+)
   #:+ (and (<= 0 i) (< i n))
       (+unsafe-list-ref e1+ e2+)
   #:- #t
       (format-bounds-error #'e1+ i)
   #:φ (L-ref l i))

(define-tailoring (-list-tail [e1 ~> e1+ (φ1 [L-dom ↦ l])]
                              [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +list-tail (τλ #'τ-list-tail #'λ-list-tail)
  #:with +unsafe-list-tail (τλ #'τ-unsafe-list-tail #'λ-unsafe-list-tail)
  #:= (or (⊥? L-dom l) (⊥? I-dom i))
      (+list-tail e1+ e2+)
  #:+ (and (<= 0 i) (< i (L-length l)))
      (+unsafe-list-tail e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) L-dom (L-slice l i (L-length l))))

(define-tailoring (-append [e* ~> e+* (φ* [L-dom l*])] ...)
  #:with +append (τλ #'τ-append #'λ-append)
  #:= (for/or ([l (in-list l*)]) (⊥? L-dom l))
      (+append e+* ...)
  #:+ #t
      (+append e+* ...)
  #:φ (φ-set (φ-init) L-dom (L-append* l*)))

(define-tailoring (-reverse [e ~> e+ (φ [L-dom l])])
  #:with +reverse (τλ #'τ-reverse #'λ-reverse)
  #:= (⊥? L-dom l)
      (+reverse e+)
  #:+ #t (+reverse e+)
  #:φ (φ-set (φ-init) L-dom (L-reverse l)))

(define-tailoring (-map [f ~> f+ (φ1 [A-dom a])]
                        [e* ~> e+* (φ* [L-dom l*])] ...)
  #:with +map (τλ #'τ-map #'λ-map)
  (define expected-arity (length l*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  (define n* (map L->I l*))
  #:= (and (⊥? I-dom (⊓* I-dom n*)) arity-ok?)
      (+map f+ e+* ...)
  #:+ arity-ok?
      (+map f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) L-dom (I->L (⊓* I-dom n*))))

(define-tailoring (-sort [e ~> e+ (φ [L-dom ↦ l])]
                         [e* ~> e+* (φ*)] ...)
  #:with +sort (τλ #'τ-sort #'λ-sort)
  #:= (⊥? L-dom l)
      (+sort e+ e+* ...)
  #:+ #t (+sort e+ e+* ...)
  #:φ (φ-set (φ-init) L-dom (if (⊥? L-dom l) l (make-φ* (L-length l)))))
