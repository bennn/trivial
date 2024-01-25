#lang racket/base

(provide
  (rename-out
    [-vector vector]
    [-build-vector build-vector]
    [-make-vector make-vector]
    [-vector-append vector-append]
    [-vector-ref vector-ref]
    [-vector-length vector-length]
    [-vector-set! vector-set!]
    [-vector-map vector-map]
    [-vector-map! vector-map!]
    [-vector->list vector->list]
    [-vector->immutable-vector vector->immutable-vector]
    [-vector-fill! vector-fill!]
    [-vector-take vector-take]
    [-vector-take-right vector-take-right]
    [-vector-drop vector-drop]
    [-vector-drop-right vector-drop-right]
    #;[-vector-split-at vector-split-at]
    #;[-vector-split-at-right vector-split-at-right]))

;; -----------------------------------------------------------------------------

(module typed-vector typed/racket
  (require
    racket/vector
    (only-in racket/unsafe/ops unsafe-vector-set!  unsafe-vector-ref))
  (provide
    λ : Integer
    vector build-vector make-vector vector-ref vector-length
    vector-append vector-set! vector-map vector-map! vector->list
    vector->immutable-vector vector-fill! vector-take vector-take-right
    vector-drop vector-drop-right unsafe-vector-set! unsafe-vector-ref))

(module untyped-vector typed/racket
  (require
    racket/vector
    (only-in racket/unsafe/ops unsafe-vector-set!  unsafe-vector-ref))
  (provide
    λ
    vector build-vector make-vector vector-ref vector-length
    vector-append vector-set! vector-map vector-map! vector->list
    vector->immutable-vector vector-fill! vector-take vector-take-right
    vector-drop vector-drop-right unsafe-vector-set! unsafe-vector-ref))

;(module optimized-vector racket
;  ;; VECTOR-MAP
;  (with-syntax ([(i* ...) (range n)])
;    (syntax/loc stx (+let ([f+ f.~>] [v+ v.~>])
;        (-vector (f+ (-vector-ref v+ 'i*)) ...))))
;  (quasisyntax/loc stx
;    (+let ([f+ f.~>] [v+ v.~>])
;      (+build-vector '#,n (+λ (#,(if (syntax-local-typed-context?)
;                                   (syntax/loc stx [i : Integer])
;                                   (syntax/loc stx i)))
;                            (f+ (+unsafe-vector-ref v+ i)))))))
;       ;; VECTOR-APPEND
;        (if (and (ok-to-unfold? (quotient n1 2)) (ok-to-unfold? (quotient n2 2)))
;             (with-syntax ([(i1* ...) (range n1)]
;                           [(i2* ...) (range n2)])
;               (syntax/loc stx
;                 (+let ([v1+ v1.~>] [v2+ v2.~>])
;                   (-vector (-vector-ref v1+ i1*) ... (-vector-ref v2+ i2*) ...))))
;             (quasisyntax/loc stx
;               (+let ([v1+ v1.~>] [v2+ v2.~>])
;                 (+build-vector '#,n1+n2
;                                (+λ (#,(if (syntax-local-typed-context?)
;                                         (syntax/loc stx [i : Integer])
;                                         (syntax/loc stx i)))
;                                  (if (< i '#,n1)
;                                    ;; TODO should use -vector-ref (but we're under a λ)
;                                    (+unsafe-vector-ref v1+ i)
;                                    (+unsafe-vector-ref v2+ i)))))))
;   VECTOR->LIST
;             (with-syntax ([(i* ...) (range n)])
;               (syntax/loc stx (+let ([v+ v.~>])
;                   (list (+unsafe-vector-ref v+ i*) ...))))
;             (quasisyntax/loc stx
;               (+let ([v+ v.~>])
;                 (build-list '#,n
;                             (+λ (#,(if (syntax-local-typed-context?)
;                                      (syntax/loc stx [i : Integer])
;                                      (syntax/loc stx i)))
;                               (+unsafe-vector-ref v+ i))))))
; VECTOR-FILL!
;          #`(+let ([v+ v.~>] [e+ e.~>])
;               (for ([i (in-range '#,n)])
;                 (+unsafe-vector-set! v+ i e+)))
; SLICE
;                (with-syntax ([hi-lo (- hi lo)])
;                     (quasisyntax/loc stx
;                       (+let ([v+ e1.~>])
;                         (-build-vector 'hi-lo
;                                        (+λ (#,(if (syntax-local-typed-context?)
;                                                (syntax/loc stx [i : Integer])
;                                                (syntax/loc stx i)))
;                                          (+unsafe-vector-ref v+ (+ i '#,lo)))))))
;)

(require
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  (prefix-in τ- 'typed-vector)
  (prefix-in λ- 'untyped-vector)
  trivial/private/function
  trivial/private/integer
  trivial/private/tailoring
  (for-syntax
    (only-in trivial/private/sequence-domain
      make-φ* format-bounds-error)
    (rename-in trivial/private/sequence-domain
      [vector-domain V-dom]
      [list-domain L-dom]
      [vector-domain->I-dom V->I]
      [I-dom->vector-domain I->V]
      [vector-domain-length V-length]
      [vector-domain-ref V-ref]
      [vector-domain-set V-set]
      [vector-domain-append* V-append*]
      [vector-domain->list-domain V->L]
      [vector-domain-slice V-slice])
    typed/untyped-utils
    syntax/parse
    racket/base
    racket/syntax
    (only-in racket/list range)
    trivial/private/common))

;; =============================================================================

(define-tailoring (-vector [e* ~> e+* (φ*)] ...)
  #:with +v (τλ #'τ-vector #'λ-vector)
  #:+ #t (+v e+* ...)
  #:φ (φ-set (φ-init) V-dom φ*))

(define-tailoring (-make-vector1 [e1 ~> e1+ (φ1 [I-dom ↦ i])])
  #:with +mv (τλ #'τ-make-vector #'λ-make-vector)
  #:= (⊥? I-dom i)
      (+mv e1+)
  #:+ #t
      (+mv e1+)
  #:φ (φ-set (φ-init) V-dom (I->V i)))

(define-tailoring (-make-vector2 [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                                 [e2 ~> e2+ (φ2)])
  #:with +mv (τλ #'τ-make-vector #'λ-make-vector)
  #:= (⊥? I-dom i)
      (+mv e1+ e2+)
  #:+ #t
      (+mv e1+ e2+)
  #:φ (φ-set (φ-init) V-dom (make-φ* i φ2 #:dom V-dom)))

(define-syntax (-make-vector stx)
  (syntax-parse stx
   [(_ e1)
    (syntax/loc stx
      (-make-vector1 e1))]
   [(_ . e*)
    (syntax/loc stx
      (-make-vector2 . e*))]
   [_:id
    #:with +mv (τλ #'τ-make-vector #'λ-make-vector)
    (syntax/loc stx
      +mv)]))

(define-tailoring (-build-vector [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                                 [e2 ~> e2+ (φ2 [A-dom ↦ a])])
  #:with +bv (τλ #'τ-build-vector #'λ-build-vector)
  (define arity-ok? (or (⊥? A-dom a) (= (length a) 1)))
  (define i-⊥? (⊥? I-dom i))
  #:= (and i-⊥? arity-ok?)
      (+bv e1+ e2+)
  #:+ (and (not i-⊥?) arity-ok?)
      (+bv e1+ e2+)
  #:- #t
      (format-arity-error #'e2+ 1)
  #:φ (φ-set (φ-init) V-dom (I->V i)))

(define-tailoring (-vector-ref [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                               [e2 ~> e2+ (φ2 [I-dom ↦ i])])
   #:with +vector-ref (τλ #'τ-vector-ref #'λ-vector-ref)
   #:with +unsafe-vector-ref (τλ #'τ-unsafe-vector-ref #'λ-unsafe-vector-ref)
   (define n (V-length v))
   #:= (or (⊥? V-dom v) (⊥? I-dom i))
       (+vector-ref e1+ e2+)
   #:+ (and (<= 0 i) (< i n))
       (+unsafe-vector-ref e1+ e2+)
   #:- #t
       (format-bounds-error #'e1+ i)
   #:φ (V-ref v i))

(define-tailoring (-vector-length [e ~> e+ (φ [V-dom v])])
  #:with +vl (τλ #'τ-vector-length #'λ-vector-length)
  (define n (V-length v))
  #:= (⊥? V-dom v)
      (+vl e+)
  #:+ #t '#,n
  #:φ (φ-set (φ-init) I-dom n))

(define-tailoring (-vector-set! [e1 ~> e1+ (φ1 [V-dom v])]
                                [e2 ~> e2+ (φ2 [I-dom i])]
                                [e3 ~> e3+ (φ3)])
  #:with +vs (τλ #'τ-vector-set! #'λ-vector-set!)
  (define n (V-length v))
  #:= (or (⊥? V-dom v) (⊥? I-dom i))
      (+vs e1+ e2+ e3+)
  #:+ (and (<= 0 i) (< i n))
      (+vs e1+ e2+ e3+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set φ1 V-dom (V-set φ1 i φ3)))

(define-tailoring (-vector-map [f ~> f+ (φ1 [A-dom a])]
                               [e* ~> e+* (φ* [V-dom v*])] ...)
  #:with +vector-map (τλ #'τ-vector-map #'λ-vector-map)
  (define n* (map V->I v*))
  (define expected-arity (length n*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  #:= (and (⊥? I-dom (⊓* I-dom n*)) arity-ok?)
      (+vector-map f+ e+* ...)
  #:+ arity-ok?
      (+vector-map f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) V-dom (I->V (⊓* I-dom n*))))

(define-tailoring (-vector-map! [f ~> f+ (φ1 [A-dom a])]
                                [e* ~> e+* (φ* [V-dom v*])] ...)
  #:with +vector-map! (τλ #'τ-vector-map! #'λ-vector-map!)
  (define n* (map V->I v*))
  (define expected-arity (length n*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  (define n-⊥? (⊥? I-dom (⊓* I-dom n*)))
  #:= (and n-⊥? arity-ok?)
      (+vector-map! f+ e+* ...)
  #:+ (and (not n-⊥?) arity-ok?)
      (+vector-map! f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) V-dom (I->V (⊓* I-dom n*))))

(define-tailoring (-vector-append [e* ~> e+* (φ* [V-dom v*])] ...)
  #:with +vector-append (τλ #'τ-vector-append #'λ-vector-append)
  (define n* (map V->I v*))
  #:= (⊥? I-dom (⊓* I-dom n*))
      (+vector-append e+* ...)
  #:+ #t
      (+vector-append e+* ...)
  #:φ (φ-set (φ-init) V-dom (V-append* v*)))

(define-tailoring (-vector->list [e ~> e+ (φ [V-dom v])])
  #:with +vector->list (τλ #'τ-vector->list #'λ-vector->list)
  #:= (⊥? V-dom v)
      (+vector->list e+)
  #:+ #t
      (+vector->list e+)
  #:φ (φ-set (φ-init) L-dom (V->L v)))

(define-tailoring (-vector->immutable-vector [e ~> e+ (φ [V-dom ↦ v])])
  #:with +vi (τλ #'τ-vector->immutable-vector #'λ-vector->immutable-vector)
  #:= (⊥? V-dom v) (+vi e+)
  #:+ #t (+vi e+)
  #:φ φ)

(define-tailoring (-vector-fill! [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                                 [e2 ~> e2+ (φ2)])
  #:with +vf (τλ #'τ-vector-fill! #'λ-vector-fill!)
  #:= (⊥? V-dom v) (+vf e1+ e2+)
  #:+ #t (+vf e1+ e2+)
  #:φ (φ-set (φ-init) V-dom (make-φ* (V-length v) φ2 #:dom V-dom)))

(define-tailoring (-vector-take [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                                [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-take #'λ-vector-take)
  (define n (V-length v))
  #:= (or (⊥? V-dom v) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (V-slice v 0 i)))

(define-tailoring (-vector-take-right [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                                      [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-take-right #'λ-vector-take-right)
  (define n (V-length v))
  #:= (or (⊥? V-dom v) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (V-slice v (reduce I-dom - n i) n)))

(define-tailoring (-vector-drop [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                                [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-drop #'λ-vector-drop)
  (define n (V-length v))
  #:= (or (⊥? V-dom v) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (V-slice v i n)))

(define-tailoring (-vector-drop-right [e1 ~> e1+ (φ1 [V-dom ↦ v])]
                                      [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-drop-right #'λ-vector-drop-right)
  (define n (V-length v))
  #:= (or (⊥? V-dom v) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (V-slice v 0 (reduce I-dom - n i))))

