#lang racket/base

(provide
  (for-syntax V-dom)
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
    (only-in racket/unsafe/ops
      unsafe-vector-set!
      unsafe-vector-ref))
  (provide
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
  (prefix-in tr- 'typed-vector)
  (only-in typed/racket λ: : Integer)
  racket/vector
  trivial/private/function
  trivial/private/integer
  (for-syntax
    typed/untyped-utils
    syntax/parse
    racket/base
    racket/syntax
    (only-in racket/list range)
    trivial/private/common))

;; =============================================================================

;; TODO maybe combine with L-dom? Because they are the same
(define-for-syntax V-dom
  (make-abstract-domain V #:leq <=
    [(~or #(e* ...) '#(e* ...))
     (length (syntax-e #'(e* ...)))]))

(define-for-syntax (bounds-error sym v i)
  (raise-user-error sym "[~a:~a] Index '~a' out of range for vector '~a'"
    (syntax-line v)
    (syntax-column v)
    i
    (syntax->datum v)))

;; -----------------------------------------------------------------------------

(define-syntax (-vector stx)
  (with-syntax ([+vector (if (syntax-local-typed-context?) (syntax/loc stx tr-vector) (syntax/loc stx vector))])
    (syntax-parse stx
     [(_ e* ...)
      (log-ttt-infer+ 'vector stx)
      (⊢ (syntax/loc stx (+vector e* ...))
         (φ-set (φ-init) V-dom (length (syntax-e #'(e* ...)))))]
     [_:id
      (syntax/loc stx +vector)])))

(define-syntax (-make-vector stx)
  (with-syntax ([+make-vector (if (syntax-local-typed-context?) (syntax/loc stx tr-make-vector) (syntax/loc stx make-vector))])
    (syntax-parse stx
     [(_ e:~> (~optional v #:defaults ([v #'0])))
      (define i (φ-ref (φ #'e.~>) I-dom))
      (cond
       [(not (integer? i))
        (log-ttt-infer- 'make-vector stx)
        (syntax/loc stx (+make-vector e.~> v))]
       [(or (< i 0) (not (fixnum? i)))
        (error 'make-vector)]
       [else
        (log-ttt-infer+ 'make-vector stx)
        (⊢ (syntax/loc stx (+make-vector e.~> v))
           (φ-set (φ-init) V-dom i))])]
     [(_ . e*)
      (syntax/loc stx (+make-vector . e*))]
     [_:id (syntax/loc stx +make-vector)])))

(define-syntax (-build-vector stx)
  (with-syntax ([+build-vector (if (syntax-local-typed-context?) (syntax/loc stx tr-build-vector) (syntax/loc stx build-vector))])
    (syntax-parse stx
     [(_ e:~> f:~>)
      (define i (φ-ref (φ #'e.~>) I-dom))
      (define arr
        (let ([arr (φ-ref (φ #'f.~>) A-dom)])
          (if (⊤? A-dom arr)
            (raise-user-error 'build-vector (⊤-msg arr))
            arr)))
      (cond
       [(and (integer? arr) (not (= arr 1)))
        (raise-user-error 'build-vector (format-arity-error (syntax/loc stx f.~>) 1))]
       [(integer? i)
        (log-ttt-infer+ 'build-vector stx)
        (⊢ (syntax/loc stx (+build-vector e.~> f.~>))
           (φ-set (φ-init) V-dom i))]
       [else
        (log-ttt-infer- 'build-vector stx)
        (syntax/loc stx (+build-vector e.~> f.~>))])]
     [(_ . e*)
      (syntax/loc stx (+build-vector . e*))]
     [_:id (syntax/loc stx +build-vector)])))

(define-syntax (-vector-ref stx)
  (with-syntax ([(+vector-ref +unsafe-vector-ref)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-ref tr-unsafe-vector-ref))
                   (syntax/loc stx (vector-ref unsafe-vector-ref)))])
    (syntax-parse stx
     [(_ e1:~> e2:~>)
      (define n (φ-ref (φ #'e1.~>) V-dom))
      (define i (φ-ref (φ #'e2.~>) I-dom))
      (cond
       [(not (and (integer? n) (integer? i)))
        (log-ttt-check- 'vector-ref stx)
        (syntax/loc stx (+vector-ref e1.~> e2.~>))]
       [(and (<= 0 i) (< i n))
        (log-ttt-check+ 'vector-ref stx)
        (syntax/loc stx (+unsafe-vector-ref e1.~> e2.~>))]
       [else
        (bounds-error 'vector-ref (syntax/loc stx e1.~>) i)])]
     [(_ . e*)
      (syntax/loc stx (+vector-ref . e*))]
     [_:id (syntax/loc stx +vector-ref)])))

(define-syntax (-vector-length stx)
  (with-syntax ([+vector-length (if (syntax-local-typed-context?) (syntax/loc stx tr-vector-length) (syntax/loc stx vector-length))])
    (syntax-parse stx
     [(_ e1:~>)
      (define n1 (φ-ref (φ #'e1.~>) V-dom))
      (cond
       [(integer? n1)
        (log-ttt-check+ 'vector-length stx)
        (⊢ (quasisyntax/loc stx '#,n1)
           (φ-set (φ-init) I-dom n1))]
       [else
        (log-ttt-check- 'vector-length stx)
        (syntax/loc stx (+vector-length e1.~>))])]
     [(_ . e*)
      (syntax/loc stx (+vector-length . e*))]
     [_:id (syntax/loc stx +vector-length)])))

(define-syntax (-vector-set! stx)
  (with-syntax ([(+vector-set! +unsafe-vector-set!)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-set! tr-unsafe-vector-set!))
                   (syntax/loc stx (vector-set! unsafe-vector-set!)))])
    (syntax-parse stx
     [(_ v:~> e:~> val:~>)
      (define n (φ-ref (φ #'v.~>) V-dom))
      (define i (φ-ref (φ #'e.~>) I-dom))
      (cond
       [(not (and (integer? n) (integer? i)))
        (log-ttt-check- 'vector-set! stx)
        (syntax/loc stx (+vector-set! v.~> e.~> val.~>))]
       [(and (<= 0 i) (< i n))
        (log-ttt-check+ 'vector-set! stx)
        (syntax/loc stx (+unsafe-vector-set! v.~> e.~> val.~>))]
       [else
        (bounds-error 'vector-set! (syntax/loc stx v) i)])]
     [(_ . e*)
      (syntax/loc stx (+vector-set! . e*))]
     [_:id (syntax/loc stx +vector-set!)])))

(define-syntax (-vector-map stx)
  (with-syntax ([(+vector-map +unsafe-vector-ref +λ +build-vector)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-map tr-unsafe-vector-ref λ: tr-build-vector))
                   (syntax/loc stx (vector-map unsafe-vector-ref λ build-vector)))])
    (syntax-parse stx
     [(_ f:~> v*:~> ...)
      (define arr
        (let ([arr (φ-ref (φ #'f.~>) A-dom)])
          (if (⊤? A-dom arr)
            (raise-user-error 'vector-map (⊤-msg arr))
            arr)))
      (define n* (for/list ([v (in-list (syntax-e #'(v*.~> ...)))])
                   (define n (φ-ref (φ v) V-dom))
                   (if (⊤? V-dom n)
                     (raise-user-error 'vector-map (⊤-msg n))
                     n)))
      (define num-vec (length n*))
      (define min-n (⊓* V-dom n*))
      (cond
       [(and (integer? arr) (not (= arr num-vec)))
        (raise-user-error 'vector-map (format-arity-error (syntax/loc stx f.~>) num-vec))]
       [else
        (if (⊥? V-dom min-n)
          (log-ttt-check- 'vector-map stx)
          (log-ttt-check+ 'vector-map stx))
        (⊢ (syntax/loc stx
             (+vector-map f.~> v*.~> ...))
           (φ-set (φ-init) V-dom min-n))])]
     [(_ . e*)
      (syntax/loc stx (+vector-map . e*))]
     [_:id (syntax/loc stx +vector-map)])))

(define-syntax (-vector-map! stx)
  (with-syntax ([(+vector-map! +unsafe-vector-ref +unsafe-vector-set!)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-map! tr-unsafe-vector-ref tr-unsafe-vector-set!))
                   (syntax/loc stx (vector-map! unsafe-vector-ref unsafe-vector-set!)))])
    (syntax-parse stx
     [(_ f:~> v*:~> ...)
      (define arr
        (let ([arr (φ-ref (φ #'f.~>) A-dom)])
          (if (⊤? A-dom arr)
            (raise-user-error 'vector-map! (⊤-msg arr))
            arr)))
      (define n* (for/list ([v (in-list (syntax-e #'(v*.~> ...)))])
                   (define n (φ-ref (φ v) V-dom))
                   (if (⊤? V-dom n)
                     (raise-user-error 'vector-map! (⊤-msg n))
                     n)))
      (define num-vec (length n*))
      (define min-n (⊓* V-dom n*))
      (cond
       [(and (integer? arr) (not (= arr num-vec)))
        (raise-user-error 'vector-map! (format-arity-error (syntax/loc stx f.~>) num-vec))]
       [else
        (if (⊥? V-dom min-n)
          (log-ttt-check- 'vector-map! stx)
          (log-ttt-check+ 'vector-map! stx))
        (⊢ (syntax/loc stx
             (+vector-map! f.~> v*.~> ...))
           (φ-set (φ-init) V-dom min-n))])]
     [(_ . e*)
      (syntax/loc stx (+vector-map! . e*))]
     [_:id (syntax/loc stx +vector-map!)])))

(define-syntax (-vector-append stx)
  (with-syntax ([(+vector-append +build-vector +λ)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-append tr-build-vector λ:))
                   (syntax/loc stx (tr-vector-append tr-build-vector λ)))])
    (syntax-parse stx
     [(_ v*:~> ...)
      (define n* (for/list ([v (in-list (syntax-e #'(v*.~> ...)))])
                   (φ-ref (φ v) V-dom)))
      (define sum-n (reduce V-dom + 0 n*))
      (cond
       [(⊤? V-dom sum-n)
        (raise-user-error 'vector-map! (⊤-msg sum-n))]
       [else
        (if (⊥? V-dom sum-n)
          (log-ttt-check- 'vector-append stx)
          (log-ttt-check+ 'vector-append stx))
        (⊢ (syntax/loc stx
             (+vector-append v*.~> ...))
           (φ-set (φ-init) V-dom sum-n))])]
     [(_ . e*)
      (syntax/loc stx (+vector-append e*))]
     [_:id (syntax/loc stx +vector-append)])))

(define-syntax (-vector->list stx)
  (with-syntax ([(+vector->list +λ +unsafe-vector-ref)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector->list λ: tr-unsafe-vector-ref))
                   (syntax/loc stx (vector->list λ unsafe-vector-ref)))])
    (syntax-parse stx
     [(_ v:~>)
      (define n (φ-ref (φ #'v.~>) V-dom))
      (cond
       [(integer? n)
        (log-ttt-check+ 'vector->list stx)
        (⊢ (syntax/loc stx
             (+vector->list v.~>))
           (φ-set (φ-init) V-dom n))]
       [else
        (log-ttt-check- 'vector->list stx)
        (syntax/loc stx (+vector->list v.~>))])]
     [(_ . e*)
      (syntax/loc stx (+vector->list . e*))]
     [_:id (syntax/loc stx +vector->list)])))

(define-syntax (-vector->immutable-vector stx)
  (with-syntax ([+vector->immutable-vector (if (syntax-local-typed-context?) (syntax/loc stx tr-vector->immutable-vector) (syntax/loc stx vector->immutable-vector))])
    (syntax-parse stx
     [(_ e:~>)
      (define n (φ-ref (φ #'e.~>) V-dom))
      (cond
       [(integer? n)
        (log-ttt-check+ 'vector->immutable-vector stx)
        (⊢ (syntax/loc stx
             (+vector->immutable-vector e.~>))
           (φ-set (φ-init) V-dom n))]
       [else
        (log-ttt-check- 'vector->immutable-vector stx)
        (syntax/loc stx (+vector->immutable-vector e.~>))])]
     [(_ . e*)
      (syntax/loc stx (+vector->immutable-vector . e*))]
     [_:id (syntax/loc stx +vector->immutable-vector)])))

(define-syntax (-vector-fill! stx)
  (with-syntax ([(+vector-fill! +unsafe-vector-set!)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-vector-fill! tr-unsafe-vector-set!))
                   (syntax/loc stx (vector-fill! unsafe-vector-set!)))])
    (syntax-parse stx
     [(_ v:~> e:~>)
      (define n (φ-ref (φ #'v.~>) V-dom))
      (cond
       [(integer? n)
        (log-ttt-check+ 'vector-fill! stx)
        (⊢ (syntax/loc stx
             (+vector-fill! v.~> e.~>))
           (φ-set (φ-init) V-dom n))]
       [else
        (log-ttt-check- 'vector-fill! stx)
        (syntax/loc stx (+vector-fill! v.~> e.~>))])]
     [(_ . e*)
      (syntax/loc stx (+vector-fill! . e*))]
     [_:id (syntax/loc stx +vector-fill!)])))

(define-syntax (define-slice-op stx)
  (syntax-parse stx
   [(_ op-name left? take?)
    #:with -op-name (format-id stx "-~a" (syntax-e #'op-name))
    #:with tr-op-name (format-id stx "tr-~a" (syntax-e #'op-name))
    #'(define-syntax (-op-name stx)
        (with-syntax ([(+op +λ +unsafe-vector-ref)
                       (if (syntax-local-typed-context?)
                         (syntax/loc stx (tr-op-name λ: tr-unsafe-vector-ref))
                         (syntax/loc stx (op-name λ unsafe-vector-ref)))])
          (syntax-parse stx
           [(_ e1:~> e2:~>)
            (define n (φ-ref (φ #'e1.~>) V-dom))
            (define i (φ-ref (φ #'e2.~>) I-dom))
            (cond
             [(and (integer? n) (integer? i))
              (define-values (lo hi)
                (if 'take?
                  (if 'left?
                    (values 0 i)
                    (values (- n i) n))
                  (if 'left?
                    (values i n)
                    (values 0 (- n i)))))
              (cond
               [(not (<= 0 i n))
                (bounds-error (syntax-e #'op-name) (syntax/loc stx v)
                  (if 'take? (if 'left? hi lo)
                             (if 'left? lo hi)))]
               [else
                (log-ttt-check+ (syntax-e #'op-name) stx)
                (⊢ (syntax/loc stx
                     (+op e1.~> e2.~>))
                   (φ-set (φ-init) V-dom (- hi lo)))])]
             [else
              (log-ttt-check- (syntax-e #'op-name) stx)
              (syntax/loc stx (+op-name e1.~> e2.~>))])]
           [(_ . e*)
            (syntax/loc stx (+op-name . e*))]
           [_:id (syntax/loc stx +op-name)])))]))

(define-slice-op vector-take       #t #t)
(define-slice-op vector-take-right #f #t)
(define-slice-op vector-drop-right #f #f)
(define-slice-op vector-drop       #t #f)

