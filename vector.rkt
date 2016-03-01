#lang typed/racket/base

;; TODO integrate with trivial/math to get ints from identifiers

(provide
  define-vector:
  let-vector:
  ;vector-length:
  vector-ref:
  ;vector-set!:
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
    unsafe-vector-ref)
  (for-syntax
    typed/racket/base
    syntax/id-table
    syntax/parse
    syntax/stx
  ))

;; =============================================================================

(define-for-syntax vector-length-key 'vector:length)
(define-for-syntax errloc-key 'vector:)
(define-for-syntax id+vector-length (make-free-id-table))

(begin-for-syntax (define-syntax-class literal-vector
  #:attributes (vector-length)
  (pattern [e* ...]
   #:with len (parse-vector-length #'(e* ...))
   #:when (syntax-e #'len)
   #:attr vector-length #'len)
))

(define-syntax (define-vector: stx)
  (syntax-parse stx
   [(_ name:id v:literal-vector)
    (free-id-table-set! id+vector-length
                        #'name
                        (syntax-e #'v.vector-length))
    #'(define name v)]
   [(_ e* ...)
    #'(define e* ...)]))

(define-syntax (let-vector: stx)
  (syntax-parse stx
   [(_ ([name:id v:literal-vector]) e* ...)
    #'(let ([name v])
        (let-syntax ([name (make-rename-transformer
                             (syntax-property #'name
                                              vector-length-key
                                              #'v.vector-length))])
          e* ...))]
   [(_ e* ...)
    #'(let e* ...)]))

(define-for-syntax (vector-ref-error v i reason)
  (raise-argument-error
    errloc-key
    (format "Index out-of-bounds: ~a" i)
    v))


(define-syntax (vector-ref: stx)
  (syntax-parse stx
   [(_ v i:nat)
    #:when (printf "ref: getting langth for ~a\n" (syntax->datum #'v))
    #:with len (parse-vector-length #'v)
    #:when (printf "ref: got langth ~a\n" (syntax->datum #'len))
    #:when (syntax-e #'len)
    (unless (< (syntax-e #'i) (syntax-e #'len))
      (vector-ref-error (syntax-e #'v) (syntax-e #'i)))
    (syntax/loc stx (unsafe-vector-ref v i))]
   [(_ e* ...)
    (syntax/loc stx (vector-ref e* ...))]))

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
            (vector e* ...))
       (length (syntax->list #'(e* ...)))]
      [(make-vector n:nat e* ...)
       (syntax-e #'n)]
      [(build-vector n:nat f)
       (syntax-e #'n)]
      [_ #f])]))


