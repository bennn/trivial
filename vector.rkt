#lang typed/racket/base

;; TODO integrate with trivial/math to get ints from identifiers

(provide
  define-vector:
  let-vector:
  vector-length:
  vector-ref:
  vector-set!:
  vector-map:
  vector-map!:
  ;vector-append:
  ;vector->list
  ;vector->immutable-vector
  ;vector-fill!
  ;

  ;; TODO and a few more

  ;; --- private
  (for-syntax parse-vector-length)
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  racket/vector
  (for-syntax
    typed/racket/base
    syntax/id-table
    syntax/parse
    syntax/stx
    trivial/private/common
  ))

;; =============================================================================

(define-for-syntax vector-length-key 'vector:length)
(define-for-syntax id+vector-length (make-free-id-table))

(begin-for-syntax (define-syntax-class vector/length
  #:attributes (expanded length)
  (pattern e
   #:with e+ (expand-expr #'e)
   #:with len (parse-vector-length #'e+)
   #:when (syntax-e #'len)
   #:attr expanded #'e+
   #:attr length #'len)
))

(define-syntax (define-vector: stx)
  (syntax-parse stx
   [(_ name:id v:vector/length)
    (free-id-table-set! id+vector-length #'name (syntax-e #'v.length))
    #'(define name v.expanded)]
   [(_ e* ...)
    #'(define e* ...)]))

(define-syntax (let-vector: stx)
  (syntax-parse stx
   [(_ ([name*:id v*:vector/length] ...) e* ...)
    #'(let ([name* v*.expanded] ...)
        (let-syntax ([name* (make-rename-transformer
                              (syntax-property #'name*
                                               vector-length-key
                                               'v*.length))] ...)
          e* ...))]
   [(_ e* ...)
    #'(let e* ...)]))

(define-for-syntax (vector-bounds-error sym v-stx i)
  (raise-syntax-error
    sym
    "Index out-of-bounds"
    (syntax->datum v-stx)
    i
    (list v-stx)))

(define-syntax (vector-length: stx)
  (syntax-parse stx
   [(_ v:vector/length)
    (syntax/loc stx 'v.length)]
   [_:id
    (syntax/loc stx vector-length)]
   [(_ e* ...)
    (syntax/loc stx (vector-length e* ...))]))

(define-syntax (vector-ref: stx)
  (syntax-parse stx
   [(_ v:vector/length i:nat)
    (unless (< (syntax-e #'i) (syntax-e #'v.length))
      (vector-bounds-error 'vector-ref: #'v (syntax-e #'i)))
    (syntax/loc stx (unsafe-vector-ref v.expanded i))]
   [_:id
    (syntax/loc stx vector-ref)]
   [(_ e* ...)
    (syntax/loc stx (vector-ref e* ...))]))

(define-syntax (vector-set!: stx)
  (syntax-parse stx
   [(_ v:vector/length i:nat val)
    (unless (< (syntax-e #'i) (syntax-e #'v.length))
      (vector-bounds-error 'vector-set!: #'v (syntax-e #'i)))
    (syntax/loc stx (unsafe-vector-set! v.expanded i val))]
   [_:id
    (syntax/loc stx vector-set!)]
   [(_ e* ...)
    (syntax/loc stx (vector-set! e* ...))]))

(define-syntax (vector-map: stx)
  (syntax-parse stx
   [(_ f v:vector/length)
    #:with (i* ...) (for/list ([i (in-range (syntax-e #'v.length))]) i)
    #:with f+ (gensym 'f)
    #:with v+ (syntax-property
                (if (small-vector-size? (syntax-e #'v.length))
                  (syntax/loc stx
                    (let ([f+ f])
                      (vector (f+ (unsafe-vector-ref v.expanded 'i*)) ...)))
                  (syntax/loc stx
                    (let ([f+ f])
                      (build-vector 'v.length (lambda ([i : Integer])
                                                (f+ (vector-ref: v.expanded i)))))))
                vector-length-key
                (syntax-e #'v.length))
    (syntax/loc stx v+)]
   [_:id
    (syntax/loc stx vector-map)]
   [(_ e* ...)
    (syntax/loc stx (vector-map e* ...))]))

(define-syntax (vector-map!: stx)
  (syntax-parse stx
   [(_ f v:vector/length)
    #:with f+ (gensym 'f)
    #:with v+ (syntax-property
                #'(let ([f+ f])
                    (for ([i (in-range 'v.length)])
                      (unsafe-vector-set! v.expanded i (f+ (unsafe-vector-ref v.expanded i))))
                    v.expanded)
                vector-length-key
                (syntax-e #'v.length))
    (syntax/loc stx v+)]
   [_:id
    (syntax/loc stx vector-map!)]
   [(_ e* ...)
    (syntax/loc stx (vector-map! e* ...))]))

;; -----------------------------------------------------------------------------

(define-for-syntax (small-vector-size? n)
  (< n 101))

;; Assume `stx` is creating a vector; get the length of the vector to-be-made
(define-for-syntax (parse-vector-length stx)
  (cond
   [(syntax-property stx vector-length-key)
    => (lambda (x) x)]
   [(identifier? stx)
    (free-id-table-ref id+vector-length stx #f)]
   [else
     (syntax-parse stx #:literals (vector make-vector build-vector)
      [(~or '#(e* ...)
            #(e* ...)
            ;; TODO #{} #[] #6{} ...
            (_ vector e* ...) ;; TODO the _ should be matching #%app
            (vector e* ...))
       (length (syntax->list #'(e* ...)))]
      [(~or (make-vector n e* ...)
            (_ make-vector n e* ...)
            (build-vector n e* ...)
            (_ build-vector n e* ...))
       (if (syntax-transforming?)
         (quoted-stx-value? (expand-expr #'n))
         (and (exact-nonnegative-integer? (syntax-e #'n)) (syntax-e #'n)))]
      [_
       #f])]))

