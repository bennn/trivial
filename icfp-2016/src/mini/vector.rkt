#lang typed/racket/base

;; Micro implementation of trivial/vector
;;  following the prose from Section 5.
;;
;; Missing:
;; - syntax-properties (for recursion)
;; - let/define
;; - predicate -> syntax class

(module A typed/racket/base
  (require (for-syntax racket/base syntax/parse))

  (begin-for-syntax
  (define ((make-alias orig-id elaborate) stx)
    (or (elaborate stx)
        (syntax-parse stx
         [_:id
          orig-id]
         [(_ e* ...)
          #`(#,orig-id e* ...)])))

  (define vector?
    (syntax-parser #:literals (make-vector)
     [(_ #(e ...))
      (length (syntax->datum #'(e ...)))]
     [_ #f]))

  (define-syntax-class vector/length
    #:attributes (evidence expanded)
    (pattern e
     #:with e+ (expand-expr #'e)
     #:with len (vector? #'e+)
     #:when (syntax->datum #'len)
     #:attr evidence #'len
     #:attr expanded #'e+))

  (define (expand-expr e)
    (local-expand e 'expression '()))
  )

  (define-syntax vector-length
    (make-alias #'vector-length
      (syntax-parser
       [(_ v:vector/length)
        #''v.evidence]
       [_ #f])))

(provide vector-length))


(require 'A)
(ann (vector-length '#(0)) One)
