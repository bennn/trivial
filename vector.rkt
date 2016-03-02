#lang typed/racket/base

;; TODO integrate with trivial/math to get ints from identifiers

(provide
  define-vector:
  let-vector:
  vector-length:
  vector-ref:
  vector-set!:
  ;vector-map:
  ;vector-append:
  ;vector->list

  ;; TODO and a few more

  ;; --- private
  (for-syntax parse-vector-length)
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  (for-syntax
    typed/racket/base
    syntax/id-table
    syntax/parse
    syntax/stx
    trivial/private/common
  ))

;; =============================================================================

(define-for-syntax vector-length-key 'vector:length)
(define-for-syntax errloc-key 'vector:)
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

(define-for-syntax (vector-bounds-error v i)
  (raise-argument-error
    errloc-key
    (format "Index out-of-bounds: ~a" i)
    v))

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
      (vector-bounds-error (syntax-e #'v) (syntax-e #'i)))
    (syntax/loc stx (unsafe-vector-ref v.expanded i))]
   [_:id
    (syntax/loc stx vector-ref)]
   [(_ e* ...)
    (syntax/loc stx (vector-ref e* ...))]))

(define-syntax (vector-set!: stx)
  (syntax-parse stx
   [(_ v:vector/length i:nat val)
    (unless (< (syntax-e #'i) (syntax-e #'v.length))
      (vector-bounds-error (syntax-e #'v) (syntax-e #'i)))
    (syntax/loc stx (unsafe-vector-set! v.expanded i val))]
   [_:id
    (syntax/loc stx vector-set!)]
   [(_ e* ...)
    (syntax/loc stx (vector-set! e* ...))]))

;; -----------------------------------------------------------------------------

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
      [(make-vector n:nat e* ...)
       (syntax-e #'n)]
      [(build-vector n:nat f)
       (syntax-e #'n)]
      [_ #f])]))

