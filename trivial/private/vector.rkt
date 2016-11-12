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
    trivial/private/sequence-domain
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
  #:φ (φ-set (φ-init) V-dom (length φ*)))

(define-tailoring (-make-vector1 [e1 ~> e1+ (φ1 [I-dom ↦ i])])
  #:with +mv (τλ #'τ-make-vector #'λ-make-vector)
  #:= (⊥? I-dom i)
      (+mv e1+)
  #:+ #t
      (+mv e1+)
  #:φ (φ-set (φ-init) V-dom i))

(define-tailoring (-make-vector2 [e1 ~> e1+ (φ1 [I-dom ↦ i])]
                                 [e2 ~> e2+ (φ2)])
  #:with +mv (τλ #'τ-make-vector #'λ-make-vector)
  #:= (⊥? I-dom i)
      (+mv e1+ e2+)
  #:+ #t
      (+mv e1+ e2+)
  #:φ (φ-set (φ-init) V-dom i))

(define-syntax (-make-vector stx)
  (syntax-parse stx
   [(_ e1)
    (syntax/loc stx
      (-make-vector1 e1))]
   [(_ . e*)
    (syntax/loc stx
      (-make-vector2 . e*))]
   [_:id
    (syntax/loc stx
      -make-vector2)]))

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
  #:φ (φ-set (φ-init) V-dom i))

(define-tailoring (-vector-ref [e1 ~> e1+ (φ1 [V-dom ↦ n])]
                               [e2 ~> e2+ (φ2 [I-dom ↦ i])])
   #:with +vector-ref (τλ #'τ-vector-ref #'λ-vector-ref)
   #:with +unsafe-vector-ref (τλ #'τ-unsafe-vector-ref #'λ-unsafe-vector-ref)
   #:= (or (⊥? V-dom n) (⊥? I-dom i))
       (+vector-ref e1+ e2+)
   #:+ (and (<= 0 i) (< i n))
       (+unsafe-vector-ref e1+ e2+)
   #:- #t
       (format-bounds-error #'e1+ i)
   #:φ (φ-init))

(define-tailoring (-vector-length [e ~> e+ (φ [V-dom n])])
  #:with +vl (τλ #'τ-vector-length #'λ-vector-length)
  #:= (⊥? V-dom n)
      (+vl e+)
  #:+ #t '#,n
  #:φ (φ-set (φ-init) I-dom n))

(define-tailoring (-vector-set! [e1 ~> e1+ (φ1 [V-dom n])]
                                [e2 ~> e2+ (φ2 [I-dom i])]
                                [e3 ~> e3+ (φ3)])
  #:with +vs (τλ #'τ-vector-set! #'λ-vector-set!)
  #:= (or (⊥? V-dom n) (⊥? I-dom i))
      (+vs e1+ e2+ e3+)
  #:+ (and (<= 0 i) (< i n))
      (+vs e1+ e2+ e3+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ φ1)

(define-tailoring (-vector-map [f ~> f+ (φ1 [A-dom a])]
                               [e* ~> e+* (φ* [V-dom n*])] ...)
  #:with +vector-map (τλ #'τ-vector-map #'λ-vector-map)
  (define expected-arity (length n*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  #:= (and (⊥? V-dom (⊓* V-dom n*)) arity-ok?)
      (+vector-map f+ e+* ...)
  #:+ arity-ok?
      (+vector-map f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) V-dom (⊓* V-dom n*)))

(define-tailoring (-vector-map! [f ~> f+ (φ1 [A-dom a])]
                                [e* ~> e+* (φ* [V-dom n*])] ...)
  #:with +vector-map! (τλ #'τ-vector-map! #'λ-vector-map!)
  (define expected-arity (length n*))
  (define arity-ok? (or (⊥? A-dom a) (= (length a) expected-arity)))
  (define n-⊥? (⊥? V-dom (⊓* V-dom n*)))
  #:= (and n-⊥? arity-ok?)
      (+vector-map! f+ e+* ...)
  #:+ (and (not n-⊥?) arity-ok?)
      (+vector-map! f+ e+* ...)
  #:- #t
      (format-arity-error #'f+ expected-arity)
  #:φ (φ-set (φ-init) V-dom (⊓* V-dom n*)))

(define-tailoring (-vector-append [e* ~> e+* (φ* [V-dom n*])] ...)
  #:with +vector-append (τλ #'τ-vector-append #'λ-vector-append)
  #:= (⊥? V-dom (⊓* V-dom n*))
      (+vector-append e+* ...)
  #:+ #t
      (+vector-append e+* ...)
  #:φ (φ-set (φ-init) V-dom (reduce* V-dom + 0 n*)))

(define-tailoring (-vector->list [e ~> e+ (φ [V-dom n])])
  #:with +vector->list (τλ #'τ-vector->list #'λ-vector->list)
  #:= (⊥? V-dom n)
      (+vector->list e+)
  #:+ #t
      (+vector->list e+)
  #:φ (φ-set (φ-init) L-dom n))

(define-tailoring (-vector->immutable-vector [e ~> e+ (φ [V-dom ↦ n])])
  #:with +vi (τλ #'τ-vector->immutable-vector #'λ-vector->immutable-vector)
  #:= (⊥? V-dom n) (+vi e+)
  #:+ #t (+vi e+)
  #:φ φ)

(define-tailoring (-vector-fill! [e ~> e+ (φ [V-dom ↦ n])])
  #:with +vf (τλ #'τ-vector-fill! #'λ-vector-fill!)
  #:= (⊥? V-dom n) (+vf e+)
  #:+ #t (+vi e+)
  #:φ φ)

(define-tailoring (-vector-take [e1 ~> e1+ (φ1 [V-dom ↦ n])]
                                [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-take #'λ-vector-take)
  #:= (or (⊥? V-dom n) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom i))

(define-tailoring (-vector-take-right [e1 ~> e1+ (φ1 [V-dom ↦ n])]
                                      [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-take-right #'λ-vector-take-right)
  #:= (or (⊥? V-dom n) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom i))

(define-tailoring (-vector-drop [e1 ~> e1+ (φ1 [V-dom ↦ n])]
                                [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-drop #'λ-vector-drop)
  #:= (or (⊥? V-dom n) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (reduce V-dom - n i)))

(define-tailoring (-vector-drop-right [e1 ~> e1+ (φ1 [V-dom ↦ n])]
                                      [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +vt (τλ #'τ-vector-drop-right #'λ-vector-drop-right)
  #:= (or (⊥? V-dom n) (⊥? I-dom i))
      (+vt e1+ e2+)
  #:+ (<= 0 i n)
      (+vt e1+ e2+)
  #:- #t
      (format-bounds-error #'e1+ i)
  #:φ (φ-set (φ-init) V-dom (reduce V-dom - n i)))

