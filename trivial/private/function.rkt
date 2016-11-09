#lang racket/base

;; Track procedure arity

;; TODO
;; - vectorized operations
;; - improve apply?
;; - get types

;; -----------------------------------------------------------------------------

(provide
  (for-syntax A-dom)
  (rename-out
   [-curry curry]
   [-lambda lambda]
   [-lambda λ]))

;; -----------------------------------------------------------------------------

(require
  (prefix-in tr- (only-in typed/racket/base lambda :))
  (for-syntax
    syntax/parse
    racket/syntax
    racket/base
    typed/untyped-utils
    trivial/private/common))

;; =============================================================================

(begin-for-syntax
  (define A-dom
    (make-abstract-domain A
     [x (⊥ A-dom)]))

  (define (parse-identifiers arr)
    (map parse-identifier arr))

  (define (parse-identifier stx)
    (syntax-parse stx #:literals (tr-:)
     [_:id stx]
     [(x tr-: _) #'x]))

  (define (curry-error stx msg)
    (raise-user-error 'curry
      "[~a:~a] ~a in ~a"
      (syntax-line stx)
      (syntax-column stx)
      msg
      (syntax->datum stx)))

)

;; -----------------------------------------------------------------------------

(define-syntax (-lambda stx)
  (with-syntax ([lam (if (syntax-local-typed-context?) (syntax/loc stx tr-lambda) (syntax/loc stx lambda))])
    (syntax-parse stx
     [(_ arg* . e*)
      (log-ttt-infer+ 'lambda stx)
      (⊢ (syntax/loc stx
           (lam arg* . e*))
         (φ-set (φ-init) A-dom (syntax-e #'arg*)))]
     [(_ . e*)
      (log-ttt-infer- 'lambda stx)
      (syntax/loc stx
        (lam . e*))]
     [_:id
      (syntax/loc stx
        lam)])))

(define-syntax (-curry stx)
  (with-syntax ([lam (if (syntax-local-typed-context?) (syntax/loc stx tr-lambda) (syntax/loc stx lambda))])
    (syntax-parse stx
     [(_ p:~>)
      (define arr (φ-ref (φ #'p.~>) A-dom))
      (cond
       [(⊥? A-dom arr)
        (log-ttt-check- 'curry stx)
        (curry-error stx "unknown arity")]
       [(⊤? A-dom arr)
        (log-ttt-check- 'curry stx)
        (curry-error stx "argument is not a function")]
       [else
        (log-ttt-check+ 'curry stx)
        (with-syntax ([id+* (parse-identifiers arr)])
          (for/fold ([expr (syntax/loc stx (p.~> . id+*))])
                    ([a (in-list (reverse arr))])
            (quasisyntax/loc stx
              (lam (#,a) #,expr)))) ])]
     [_
      (log-ttt-check- 'curry stx)
      (curry-error stx "bad syntax")])))

