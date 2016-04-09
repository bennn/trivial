#lang typed/racket/base

;; Statically-checked format strings

(provide
  format:
  printf:

  (for-syntax
    format-define
    format-let)
)

(require
  (for-syntax
    trivial/private/common
    typed/racket/base
    syntax/parse))

;; =============================================================================

(begin-for-syntax
  ;; Count the number of format escapes in a string.
  ;; Returns a list of optional types (to be spliced into the source code).
  ;;   Example: If result is '(#f Integer), then
  ;;   - The format string expects 2 arguments
  ;;   - First argument has no type constraints, second must be an Integer
  (define (format-parser stx)
    (define str (if (string? (syntax-e stx)) (syntax-e stx) (quoted-stx-value? stx)))
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
            (loop (+ i 2) (cons (syntax/loc stx Char) acc))]
           [(#\b #\B #\o #\O #\x #\X)
            ;; Need 1 `exact?`
            (loop (+ i 2) (cons (syntax/loc stx Exact-Number) acc))]
           [else
            ;; Invalid format sequence
            (raise-user-error "format: unrecognized pattern string '~~~c'"
              (string-ref str (+ i 1)))])]
         [else
          (loop (+ i 1) acc)]))]
     [else #f]))

  (define-values (_key fmt? format-define format-let)
    (make-value-property 'string:format format-parser))

  (define-syntax-class/predicate string/format fmt?)
)

;; -----------------------------------------------------------------------------

(define-syntax format: (make-alias #'format
  (lambda (stx) (syntax-parse stx
   [(_ fmt:string/format arg* ...)
    ;; -- 1. Parse expected types from the template
    #:when (let ([num-expected (length (syntax-e #'fmt.evidence))]
                 [num-given (length (syntax-e #'(arg* ...)))])
             (unless (= num-expected num-given)
               (apply raise-arity-error
                 'format:
                 num-expected
                 (map syntax->datum (syntax-e #'(arg* ...))))))
    ;; -- 2. If any types left obligations, use `ann` to typecheck the args
    #:with (arg+* ...)
      (for/list ([a (in-list (syntax-e #'(arg* ...)))]
                 [t (in-list (syntax-e #'fmt.evidence))])
        (if (syntax-e t) (quasisyntax/loc stx (ann #,a #,t)) a))
    (syntax/loc stx (format fmt.expanded arg+* ...))]
   [_ #f]))))

(define-syntax printf: (make-alias #'printf
  (lambda (stx) (syntax-parse stx
   [(_ arg* ...)
    (syntax/loc stx (display (format: arg* ...)))]
   [_ #f]))))

