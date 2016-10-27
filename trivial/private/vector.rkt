#lang typed/racket/base

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

(require
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  racket/vector
  trivial/private/integer
  (for-syntax
    syntax/parse
    racket/base
    racket/syntax
    (only-in racket/list range)
    trivial/private/common))

;; =============================================================================

(define-for-syntax V-dom
  (make-abstract-domain V
    [(~or #(e* ...) '#(e* ...))
     (length (syntax-e #'(e* ...)))]))

(define-for-syntax (bounds-error sym v i)
  (raise-user-error sym "Index '~a' out of range for vector '~a'" i v))

;; -----------------------------------------------------------------------------

(define-syntax (-vector stx)
  (syntax-parse stx
   [(_ e* ...)
    (⊢ (syntax/loc stx (vector e* ...))
       (φ-set (φ-init) V-dom (length (syntax-e #'(e* ...)))))]
   [_:id (syntax/loc stx vector)]))

(define-syntax (-make-vector stx)
  (syntax-parse stx
   [(_ e:~> (~optional v #:defaults ([v #'0])))
    (define i (φ-ref (φ #'e.~>) I-dom))
    (cond
     [(not (integer? i))
      (syntax/loc stx (make-vector e.~> v))]
     [(or (< i 0) (not (fixnum? i)))
      (error 'make-vector)]
     [else
      (⊢ (syntax/loc stx (make-vector e.~> v)) (φ-set (φ-init) V-dom i))])]
   [(_ . e*)
    (syntax/loc stx (make-vector . e*))]
   [_:id (syntax/loc stx make-vector)]))

(define-syntax (-build-vector stx)
  (syntax-parse stx
   [(_ e:~> f:~>)
    (define i (φ-ref (φ #'e.~>) I-dom))
    (if (integer? i)
      (⊢ (syntax/loc stx (build-vector e.~> f.~>))
         (φ-set (φ-init) V-dom i))
      (syntax/loc stx (build-vector e.~> f.~>)))]
   [(_ . e*)
    (syntax/loc stx (build-vector . e*))]
   [_:id (syntax/loc stx build-vector)]))

(define-syntax (-vector-ref stx)
  (syntax-parse stx
   [(_ e1:~> e2:~>)
    (define n (φ-ref (φ #'e1.~>) V-dom))
    (define i (φ-ref (φ #'e2.~>) I-dom))
    (cond
     [(not (and (integer? n) (integer? i)))
      (syntax/loc stx (vector-ref e1.~> e2.~>))]
     [(and (<= 0 i) (< i n))
      (syntax/loc stx (unsafe-vector-ref e1.~> e2.~>))]
     [else
      (bounds-error 'vector-ref (syntax/loc stx e1.~>) i)])]
   [(_ . e*)
    (syntax/loc stx (vector-ref . e*))]
   [_:id (syntax/loc stx vector-ref)]))

(define-syntax (-vector-length stx)
  (syntax-parse stx
   [(_ e1:~>)
    (define n1 (φ-ref (φ #'e1.~>) V-dom))
    (if (integer? n1)
      (⊢ (quasisyntax/loc stx '#,n1)
         (φ-set (φ-init) I-dom n1))
      (syntax/loc stx (vector-length e1.~>)))]
   [(_ . e*)
    (syntax/loc stx (vector-length . e*))]
   [_:id (syntax/loc stx vector-length)]))

(define-syntax (-vector-set! stx)
  (syntax-parse stx
   [(_ v:~> e:~> val:~>)
    (define n (φ-ref (φ #'v.~>) V-dom))
    (define i (φ-ref (φ #'e.~>) I-dom))
    (cond
     [(not (and (integer? n) (integer? i)))
      (syntax/loc stx (vector-set! v.~> e.~> val.~>))]
     [(and (<= 0 i) (< i n))
      (syntax/loc stx (unsafe-vector-set! v.~> e.~> val.~>))]
     [else
      (bounds-error 'vector-set! (syntax/loc stx v) i)])]
   [(_ . e*)
    (syntax/loc stx (vector-set! . e*))]
   [_:id (syntax/loc stx vector-set!)]))

(define-syntax (-vector-map stx)
  (syntax-parse stx
   [(_ f:~> v:~>)
    #:with f+ (format-id #f "f" #:source stx)
    #:with v+ (format-id #f "v" #:source stx)
    (define n (φ-ref (φ #'v.~>) V-dom))
    (cond
     [(integer? n)
      (⊢ (if (ok-to-unfold? n)
           (with-syntax ([(i* ...) (range n)])
             (syntax/loc stx (let ([f+ f.~>] [v+ v.~>])
                 (-vector (f+ (-vector-ref v+ 'i*)) ...))))
           (quasisyntax/loc stx (let ([f+ f.~>] [v+ v.~>])
               (build-vector '#,n (λ ([i : Integer]) (f+ (unsafe-vector-ref v+ i)))))))
         (φ-set (φ-init) V-dom n))]
     [else
      (syntax/loc stx (vector-map f.~> v.~>))])]
   [(_ . e*)
    (syntax/loc stx (vector-map . e*))]
   [_:id (syntax/loc stx vector-map)]))

(define-syntax (-vector-map! stx)
  (syntax-parse stx
   [(_ f:~> v:~>)
    #:with f+ (format-id #f "f" #:source stx)
    #:with v+ (format-id #f "v" #:source stx)
    (define n (φ-ref (φ #'v.~>) V-dom))
    (cond
     [(integer? n)
      (⊢ #`(let ([f+ f.~>] [v+ v.~>])
             (for ([i (in-range '#,n)])
               (unsafe-vector-set! v+ i (f+ (unsafe-vector-ref v+ i))))
             v+)
         (φ-set (φ-init) V-dom n))]
     [else
      (syntax/loc stx (vector-map! f.~> v.~>))])]
   [(_ . e*)
    (syntax/loc stx (vector-map! . e*))]
   [_:id (syntax/loc stx vector-map!)]))

(define-syntax (-vector-append stx)
  (syntax-parse stx
   [(_ v1:~> v2:~>)
    #:with v1+ (format-id #f "v1" #:source stx)
    #:with v2+ (format-id #f "v2" #:source stx)
    (define n1 (φ-ref (φ #'v1.~>) V-dom))
    (define n2 (φ-ref (φ #'v2.~>) V-dom))
    (cond
     [(and (integer? n1) (integer? n2))
      (define n1+n2 (+ n1 n2))
      (⊢ (if (and (ok-to-unfold? (quotient n1 2)) (ok-to-unfold? (quotient n2 2)))
           (with-syntax ([(i1* ...) (range n1)]
                         [(i2* ...) (range n2)])
             (syntax/loc stx (let ([v1+ v1.~>] [v2+ v2.~>])
                 (-vector (-vector-ref v1+ i1*) ... (-vector-ref v2+ i2*) ...))))
           (syntax/loc stx (let ([v1+ v1.~>] [v2+ v2.~>])
               (build-vector #,(+ i1 i2)
                             (λ ([i : Integer])
                               (if (< i '#,i1)
                                 ;; TODO should use -vector-ref (under a λ)
                                 (unsafe-vector-ref v1+ i)
                                 (unsafe-vector-ref v2+ i)))))))
         (φ-set (φ-init) V-dom n1+n2))]
     [else
      (syntax/loc stx (vector-append v1.~> v2.~>))])]
   [(_ . e*)
    (syntax/loc stx (vector-append e*))]
   [_:id (syntax/loc stx vector-append)]))

(define-syntax (-vector->list stx)
  (syntax-parse stx
   [(_ v:~>)
    #:with v+ (format-id #f "v" #:source stx)
    (define n (φ-ref (φ #'v.~>) V-dom))
    (cond
     [(integer? n)
      (⊢ (if (ok-to-unfold? n)
           (with-syntax ([(i* ...) (range n)])
             (syntax/loc stx (let ([v+ v.~>])
                 (list (-vector-ref v+ i*) ...))))
           #`(let ([v+ v.~>])
               (build-list '#,n (λ ([i : Integer]) (unsafe--vector-ref v+ i)))))
         (φ-set (φ-init) V-dom n))]
     [else
      (syntax/loc stx (vector->list v.~>))])]
   [(_ . e*)
    (syntax/loc stx (vector->list . e*))]
   [_:id (syntax/loc stx vector->list)]))

(define-syntax (-vector->immutable-vector stx)
  (syntax-parse stx
   [(_ e:~>)
    (define n (φ-ref (φ #'e.~>) V-dom))
    (if (integer? n)
      (⊢ (syntax/loc stx (vector->immutable-vector e.~>))
         (φ-set (φ-init) V-dom n))
      (syntax/loc stx (vector->immutable-vector e.~>)))]
   [(_ . e*)
    (syntax/loc stx (vector->immutable-vector . e*))]
   [_:id (syntax/loc stx vector->immutable-vector)]))

(define-syntax (-vector-fill! stx)
  (syntax-parse stx
   [(_ v:~> e:~>)
    #:with v+ (format-id #f "v" #:source stx)
    #:with e+ (format-id #f "e" #:source stx)
    (define n (φ-ref (φ #'v.~>) V-dom))
    (cond
     [(integer? n)
      (⊢ #`(let ([v+ v.~>] [e+ e.~>])
             (for ([i (in-range '#,n)])
               (unsafe-vector-set! v+ i e+)))
         (φ-set (φ-init) V-dom n))]
     [else
      (syntax/loc stx (vector-fill! v.~> e.~>))])]
   [(_ . e*)
    (syntax/loc stx (vector-fill! . e*))]
   [_:id (syntax/loc stx vector-fill!)]))

(begin-for-syntax (define-syntax-rule (make-slice-op left? take?)
  (λ (stx)
    (syntax-parse stx
     [(op-name e1:~> e2:~>)
      #:with v+ (format-id #f "v" #:source stx)
      #:with n+ (format-id #f "n" #:source stx)
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
        (if (not (<= 0 i n))
          (bounds-error (syntax-e #'op-name) (syntax/loc stx v)
            (if 'take? (if 'left? hi lo)
                       (if 'left? lo hi)))
          (⊢ (with-syntax ([hi-lo (- hi lo)])
               (quasisyntax/loc stx
                 (let ([v+ e1.~>])
                   (-build-vector 'hi-lo
                                  (λ ([i : Integer])
                                    (unsafe-vector-ref v+ (+ i '#,lo)))))))
             (φ-set (φ-init) V-dom (- hi lo))))]
       [else
        (syntax/loc stx (op-name e1.~> e2.~>))])]
     [(_ . e*)
      (syntax/loc stx (op-name . e*))]
     [_:id (syntax/loc stx op-name)]))))

(define-syntax -vector-take       (make-slice-op #t #t))
(define-syntax -vector-take-right (make-slice-op #f #t))
(define-syntax -vector-drop-right (make-slice-op #f #f))
(define-syntax -vector-drop       (make-slice-op #t #f))


