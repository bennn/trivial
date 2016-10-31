#lang racket/base

;; Integer arithmetic + contant propagation

(provide
  (for-syntax I-dom)
  (rename-out
   [-+ +]
   [-- -]
   [-* *]
   [-/ /]
   [-add1 add1]
   [-sub1 sub1]
   [-expt expt]
   [-quotient quotient])
)

;; -----------------------------------------------------------------------------

(require
  syntax/parse
  (for-syntax
    syntax/parse
    racket/syntax
    racket/base
    trivial/private/common))

;; =============================================================================

(define-for-syntax I-dom
  (make-abstract-domain I
   [i:integer
    (syntax-e #'i)]))

(define-for-syntax (division-by-zero stx)
  (raise-syntax-error '/ "division by zero" stx))

;; Simplify a list of expressions using an associative binary operator.
;; Return either:
;; - A numeric value
;; - A list of syntax objects, to be spliced back in the source code
(define-for-syntax (reduce/op op stx)
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
         [e+:~>
          (define v (φ-ref (φ #'e+.~>) I-dom))
          (if (integer? v)
            ;; then: reduce the number
            (if prev
              ;; Watch for division-by-zero
              (if (and (zero? v) (eq? / op))
                (division-by-zero stx)
                (loop (op prev v) acc (cdr e*)))
              (loop v acc (cdr e*)))
            ;; else: save value in acc
            (let ([acc+ (cons #'e+.~> (if prev (cons prev acc) acc))])
              (loop #f acc+ (cdr e*))))])))]
   [else  #f]))

(define-syntax make-numeric-operator
  (syntax-parser
   [(_ f:id)
    #:with -f (format-id #'f "-~a" (syntax-e #'f))
    #'(define-syntax (-f stx)
        (syntax-parse stx
         [(_ e* (... ...))
          #:with f-id (format-id stx "~a" 'f)
          (let ([e+ (reduce/op f #'(e* (... ...)))])
            (cond
             [(list? e+)
              (log-ttt-check- 'f stx)
              (quasisyntax/loc stx (#%app f-id #,@e+))]
             [else
              (log-ttt-check+ 'f stx)
              (⊢ (quasisyntax/loc stx #,e+)
                 (φ-set (φ-init) I-dom e+))]))]
         [_:id #'f]))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

(define-syntax (-add1 stx)
  ;(make-lifted-function add1 I-dom)
    (syntax-parse stx
     [(_ e:~>)
      (define φ-e (φ #'e.~>))
      (define v (φ-ref φ-e I-dom))
      (cond
       [(⊤? I-dom v)
        (raise-user-error 'add1 (⊤-msg v))]
       [else
        (define φ+ (if (⊥? I-dom v) φ-e (φ-set φ-e I-dom (add1 v))))
        (⊢ (syntax/loc stx (add1 e.~>))
           φ+)])]
     [(_ . e*)
      (syntax/loc stx (add1 . e*))]
     [_:id
      (syntax/loc stx add1)]))

(define-syntax (-sub1 stx)
  ;(make-lifted-function sub1 I-dom)
    (syntax-parse stx
     [(_ e:~>)
      (define φ-e (φ #'e.~>))
      (define v (φ-ref φ-e I-dom))
      (cond
       [(⊤? I-dom v)
        (raise-user-error 'sub1 (⊤-msg v))]
       [else
        (⊢ (syntax/loc stx (sub1 e.~>))
           (if (⊥? I-dom v) φ-e (φ-set φ-e I-dom (sub1 v))))])]
     [(_ . e*)
      (syntax/loc stx (sub1 . e*))]
     [_:id
      (syntax/loc stx sub1)]))

(define-syntax (-expt stx)
  (syntax-parse stx
   [(_ n1:~> n2:~>)
    (define n1-val (φ-ref (φ #'n1.~>) I-dom))
    (define n2-val (φ-ref (φ #'n2.~>) I-dom))
    (cond
     [(and (integer? n1-val) (integer? n2-val))
      (log-ttt-check+ 'expt stx)
      (define n1n2 (expt n1-val n2-val))
      (⊢ (quasisyntax/loc stx '#,n1n2)
         (φ-set (φ-init) I-dom '#,n1n2))]
     [(and (integer? n2-val) (ok-to-unfold? n2-val))
      (cond
       [(zero? n2-val)
        (log-ttt-check+ 'expt stx) ;; close enough
        (⊢ (quasisyntax/loc stx 1)
           (φ-set (φ-init) I-dom 1))]
       [else
        (log-ttt-check+ 'expt stx) ;; close enough
        (quasisyntax/loc stx
          (* #,@(for/list ([_i (in-range n2-val)]) (quasisyntax/loc stx n1))))])]
     [else
      (log-ttt-check- 'expt stx)
      #'(expt n1.~> n2.~>)])]
   [(_ . n*)
    #'(expt . n*)]
   [_:id #'expt]))

(define-syntax (-quotient stx)
  (syntax-parse stx
   [(_ n1:~> n2:~>)
    (define n1-val (φ-ref (φ #'n1.~>) I-dom))
    (define n2-val (φ-ref (φ #'n2.~>) I-dom))
    (cond
     [(and (integer? n1-val) (integer? n2-val))
      (log-ttt-check+ 'quotient stx)
      (define n1/n2 (quotient n1-val n2-val))
      (⊢ (quasisyntax/loc stx '#,n1/n2)
         (φ-set (φ-init) I-dom '#,n1/n2))]
     [else
      (log-ttt-check- 'quotient stx)
      #'(quotient n1.~> n2.~>)])]
   [(_ . n*)
    #'(quotient . n*)]
   [_:id #'quotient]))
