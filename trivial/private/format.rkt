#lang racket/base

;; Statically-checked format strings

(provide
  (for-syntax F-dom)
  (rename-out
   [-format format]
   [-printf printf]))

(require
  (prefix-in τ- (only-in typed/racket/base ann format printf Char Exact-Number))
  (prefix-in λ- (only-in racket/base format printf))
  trivial/private/tailoring
  (for-syntax
    syntax/parse
    racket/syntax
    racket/base
    typed/untyped-utils
    trivial/private/common))

;; =============================================================================

(begin-for-syntax
  (define F-dom
    (make-abstract-domain F
     [s:str
      (parse-formats #'s)]))

  ;; By analogy to 'format-group-error' ...
  (define (format-format-error stx str i)
    (format
      "[~a:~a] Invalid format escape at position ~a in '~a'"
      (syntax-line stx)
      (syntax-column stx)
      i
      str))

  (define (format-format-arity-error stx expect)
    (format
      "[~a:~a] format string expected ~a argument~a, given ~a"
      (syntax-line stx)
      (syntax-column stx)
      expect
      (if (equal? 1 expect) "" "s")
      (syntax->datum stx)))

  ;; Count the number of format escapes in a string.
  ;; Returns a list of optional types (to be spliced into the source code).
  ;;   Example: If result is '(#f Integer), then
  ;;   - The format string expects 2 arguments
  ;;   - First argument has no type constraints, second must be an Integer
  (define (parse-formats stx)
    (define str (syntax-e stx))
    (cond
     [(string? str)
      (define last-index (- (string-length str) 1))
      (let loop ([i 0] [acc '()])
        (cond
         [(>= i last-index)
          (reverse acc)]
         [(eq? #\~ (string-ref str i))
          ;; From fprintf docs @ http://docs.racket-lang.org/reference/Writing.html
          (case (string-ref str (+ i 1))
           [(#\% #\n #\~ #\space #\tab #\newline)
            ;; Need 0 arguments
            (loop (+ i 2) acc)]
           [(#\a #\A #\s #\S #\v #\V #\e #\E)
            ;; Need 1 argument, can be anything
            (loop (+ i 2) (cons #f acc))]
           [(#\.)
            ;; Need at most 1, can be anything
            (if (and (< (+ 1 i) last-index)
                     (memq (string-ref str (+ i 2)) '(#\a #\A #\s #\S #\v #\V)))
              (loop (+ i 3) (cons #f acc))
              (loop (+ i 3) acc))]
           [(#\c #\C)
            ;; Need 1 `char?`
            (loop (+ i 2) (cons 'τ-Char acc))]
           [(#\b #\B #\o #\O #\x #\X)
            ;; Need 1 `exact?`
            (loop (+ i 2) (cons 'τ-Exact-Number acc))]
           [else
            ;; Invalid format sequence
            (⊤ F-dom (format-format-error stx str i))])]
         [else
          (loop (+ i 1) acc)]))]
     [else (⊥ F-dom)]))

)

;; -----------------------------------------------------------------------------

(define-syntax (define-formatter stx)
  (syntax-parse stx
   [(_ -fmt:id)
    #:with λ-fmt (format-id stx "λ~a" #'-fmt)
    #:with τ-fmt (format-id stx "τ~a" #'-fmt)
    (syntax/loc stx
      (define-tailoring (-fmt [e1 ~> e1+ (φ1 [F-dom ↦ fmt?])]
                              [e* ~> e+* (φ*)] (... ...))
        #:with +f (τλ #'τ-fmt #'λ-fmt)
        (define num-given (length φ*))
        (define typed-context? (syntax-local-typed-context?))
        #:= (⊥? F-dom fmt?)
            (+f e1+ e+* (... ...))
        #:+ (= num-given (length fmt?))
            (+f e1+ #,@(for/list ([a (in-list (syntax-e #'(e* (... ...))))]
                                  [t (in-list fmt?)])
                         (if (and t typed-context?)
                           #`(τ-ann #,a #,t)
                           a)))
        #:- #t
            (format-format-arity-error #'(e+* (... ...)) (length fmt?))
        #:φ (φ-init)))]))

(define-formatter -format)
(define-formatter -printf)
