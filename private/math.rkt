#lang typed/racket/base

;; Constant-folding math operators.
;; Where possible, they simplify their arguments.

;; TODO the or- stuff is not so pretty, but it's working anyway

(provide
  +: -: *: /:
  ;; Same signature as the racket/base operators,
  ;;  but try to simplify arguments during expansion.

  expt:

  define-num: let-num:

  ;; --
  (for-syntax
    nat/expand
    int/expand
    num/expand)
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

(begin-for-syntax
  (define (division-by-zero stx)
    (raise-syntax-error '/ "division by zero" stx))

  ;; Simplify a list of expressions using an associative binary operator.
  ;; Return either:
  ;; - A numeric value
  ;; - A list of syntax objects, to be spliced back in the source code
  (define (reduce/op op stx)
    (define expr* (syntax-e stx))
    (cond
     [(list? expr*)
      (let loop ([prev #f]      ;; (U #f Number), candidate for reduction
                 [acc  '()]     ;; (Listof Syntax), irreducible arguments
                 [e*   expr*])  ;; (Listof Syntax), arguments to process
        (if (null? e*)
          ;; then: finished, return a number (prev) or list of expressions (acc)
          (if (null? acc)
            prev
            (reverse (if prev (cons prev acc) acc)))
          ;; else: pop the next argument from e*, fold if it's a constant
          (syntax-parse (car e*)
           [v:num/expand
            (define v (or (quoted-stx-value? #'v.expanded)
                          (quoted-stx-value? #'v.evidence)))
            ;; then: reduce the number
            (if prev
              ;; Watch for division-by-zero
              (if (and (zero? v) (eq? / op))
                (division-by-zero stx)
                (loop (op prev v) acc (cdr e*)))
              (loop v acc (cdr e*)))]
           [v
            ;; else: save value in acc
            (let ([acc+ (cons (car e*) (if prev (cons prev acc) acc))])
              (loop #f acc+ (cdr e*)))])))]
     [else  #f]))

  (define-values (nat-key nat? nat-define nat-let)
    (make-value-property 'number:natural (lift-predicate exact-nonnegative-integer?)))
  (define-syntax-class/predicate nat/expand nat?)

  (define-values (int-key int? int-define int-let)
    (make-value-property 'number:integer (lift-predicate integer?)))
  (define-syntax-class/predicate int/expand int?)

  (define-values (num-key num? num-define num-let)
    (make-value-property 'number:number (lift-predicate number?)))
  (define-syntax-class/predicate num/expand num?)
)

;; -----------------------------------------------------------------------------

(define-syntax define-num: (make-alias 'define num-define))
(define-syntax let-num: (make-alias 'let num-let))

(define-syntax make-numeric-operator
  (syntax-parser
   [(_ f:id)
    #:with f: (format-id #'f "~a:" (syntax-e #'f))
    #'(define-syntax f: (make-alias #'f
        (lambda (stx) (syntax-parse stx
         [(_ e* (... ...))
          #:with f-id (format-id stx "~a" 'f)
          (let ([e+ (reduce/op f #'(e* (... ...)))])
            (if (list? e+)
              (quasisyntax/loc stx (#%app f-id #,@e+))
              (quasisyntax/loc stx #,e+)))]
         [_ #f]))))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

(define-syntax expt: (make-alias 'expt
  (lambda (stx) (syntax-parse stx
   [(_ n1:num/expand n2:num/expand)
    (let ([v1 (or (quoted-stx-value? #'n1.expanded)
                  (quoted-stx-value? #'n1.evidence))]
          [v2 (or (quoted-stx-value? #'n2.expanded)
                  (quoted-stx-value? #'n2.evidence))])
      (and v1 v2 ;; Should never fail
        (quasisyntax/loc stx #,(expt v1 v2))))]
   [_ #f]))))
