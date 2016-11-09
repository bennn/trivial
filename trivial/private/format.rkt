#lang racket/base

;; Statically-checked format strings

(provide
  (for-syntax F-dom)
  (rename-out
   [-format format]
   [-printf printf]))

(require
  (prefix-in tr- (only-in typed/racket/base ann format printf Char Exact-Number))
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
            (loop (+ i 2) (cons (syntax/loc stx tr-Char) acc))]
           [(#\b #\B #\o #\O #\x #\X)
            ;; Need 1 `exact?`
            (loop (+ i 2) (cons (syntax/loc stx tr-Exact-Number) acc))]
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
   [(_ fmt:id)
    #:with -fmt (format-id stx "-~a" (syntax-e #'fmt))
    #:with tr-fmt (format-id stx "tr-~a" (syntax-e #'fmt))
    #'(define-syntax (-fmt stx)
        (let ([typed-context? (syntax-local-typed-context?)])
          (with-syntax ([fmt (if typed-context? (syntax/loc stx tr-fmt) (syntax/loc stx fmt))])
            (syntax-parse stx
             [(_ s:~> . e*)
              (define fmt? (φ-ref (φ #'s.~>) F-dom))
              (cond
               [(⊥? F-dom fmt?)
                (log-ttt-check- 'fmt stx)
                (syntax/loc stx
                  (fmt s.~> . e*))]
               [(⊤? F-dom fmt?)
                (raise-user-error 'fmt (⊤-msg fmt?))]
               [else
                (log-ttt-check+ 'fmt stx)
                (define num-given (length (syntax-e #'e*)))
                (define num-expected (length fmt?))
                (unless (= num-expected num-given)
                  (apply raise-arity-error 'fmt num-expected (map syntax->datum (syntax-e #'e*))))
                (with-syntax ([arg+*
                               (for/list ([a (in-list (syntax-e #'e*))]
                                          [t (in-list fmt?)])
                                 (if (and t (syntax-e t) typed-context?)
                                   (quasisyntax/loc stx (tr-ann #,a #,t))
                                   a))])
                  (syntax/loc stx
                    (fmt s.~> . arg+*)))])]
             [(_ . e*)
              (syntax/loc stx (fmt . e*))]
             [_:id
              (syntax/loc stx fmt)]))))]))

(define-formatter format)
(define-formatter printf)
