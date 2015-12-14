#lang typed/racket/base

;; Constant-folding math operators.
;; Where possible, they simplify their arguments.

(provide
  +: -: *: /:
  ;; Same signature as the racket/base operators,
  ;;  but try to simplify arguments during expansion.
)

(require (for-syntax
  typed/racket/base
  (only-in racket/format ~a)
  (only-in racket/syntax format-id)
  syntax/id-table
  syntax/parse
  trivial/private/common
))

;; =============================================================================

(define-syntax make-numeric-operator
  (syntax-parser
   [(_ f:id)
    #:with f: (format-id #'f "~a:" (syntax-e #'f))
    #'(define-syntax f:
        (syntax-parser
         [(g e* (... ...))
          #:with e+* (for/list ([e (in-list (syntax->list #'(e* (... ...))))])
                       (expand-expr e))
          (let ([e++ (reduce/op f (syntax->list #'e+*))])
            (if (list? e++)
              (quasisyntax/loc #'g (f #,@e++))
              (quasisyntax/loc #'g #,e++)))]
         [g:id
          (syntax/loc #'g f)]
         [(g e* (... ...))
          (syntax/loc #'g (f e* (... ...)))]))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

;; -----------------------------------------------------------------------------

;; Simplify a list of expressions using an associative binary operator.
;; Return either:
;; - A numeric value
;; - A list of syntax objects, to be spliced back in the source code
(define-for-syntax (reduce/op op e*)
  (let loop ([prev #f]   ;; (U #f Number), candidate for reduction
             [acc  '()]  ;; (Listof Syntax), irreducible arguments
             [e*   e*])  ;; (Listof Syntax), arguments to process
    (if (null? e*)
      ;; then: finished, return a number (prev) or list of expressions (acc)
      (if (null? acc)
        prev
        (reverse (if prev (cons prev acc) acc)))
      ;; else: pop the next argument from e*, fold if it's a constant
      (let ([v (quoted-stx-value? (car e*))])
        (if (number? v)
          ;; then: reduce the number
          (if prev
            ;; Watch for division-by-zero
            (if (and (zero? v) (eq? / op))
              (loop v (cons prev acc) (cdr e*))
              (loop (op prev v) acc (cdr e*)))
            (loop v acc (cdr e*)))
          ;; else: save value in acc
          (let ([acc+ (cons (car e*) (if prev (cons prev acc) acc))])
            (loop #f acc+ (cdr e*))))) )))
