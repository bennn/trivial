#lang typed/racket/base

(provide
  define-vector:
  ;let-vector:
  ;vector-length:
  ;vector-ref:
  ;vector-set!:
  ;vector-map:
  ;vector-append:

  ;; TODO and a few more

  ;; --- private
  (for-syntax parse-vector-length)
)

;; -----------------------------------------------------------------------------

(require
  (for-syntax
    typed/racket/base
    syntax/id-table
    syntax/parse
    syntax/stx
  ))

;; =============================================================================

(define-for-syntax vec-length-key 'vector:length)
(define-for-syntax errloc-key 'vector:)
(define-for-syntax id+vector-length (make-free-id-table))

(define-syntax (define-vector: stx)
  (syntax-parse stx
   [(_ name:id v)
    #:with len (parse-vector-length #'v)
    #:when (syntax-e #'len)
    (free-id-table-set! id+vector-length
                        #'name
                        (syntax-e #'len))
    #'(define name v)]
   [(_ e* ...)
    #'(define e* ...)]))

;; Assume `stx` is creating a vector; get the length of the vector to-be-made
(define-for-syntax (parse-vector-length stx)
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
   [_ #f]))

