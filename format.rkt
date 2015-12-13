#lang typed/racket/base

(provide
  format:
  ;; (-> (x:String) Any *N Void)
  ;; Takes 1 required string argument and N additional arguments,
  ;;  where N is the number of format sequences in the string.
  ;;
  ;; If the string is a literal, raises a compile-time arity error if
  ;;  the given number of arguments does not match the format string.
  ;;
  ;; If the string is not a literal, arity-checking happens at runtime.

  printf:
  ;; (-> (x:String) Any *N Void)
  ;; Similar to `format`, but displays the formatted string to `current-output-port`.
)

(require
  (for-syntax typed/racket/base syntax/parse racket/sequence))

;; =============================================================================

(define-syntax format:
  (syntax-parser
   [(f template:str arg* ...)
    ;; 1. Parse expected types from the template
    (let* ([type* (template->type* (syntax-e #'template) #:src #'f)]
           [num-expected (length type*)]
           [num-given (for/sum ([a (in-syntax #'(arg* ...))]) 1)])
      (unless (= num-expected num-given)
        (raise-arity-error
          (syntax-e #'f)
          num-expected
          (for/list ([a (in-syntax #'(arg* ...))]) (syntax->datum a))))
      ;; 2. If any types left obligations, use `ann` to typecheck the args
      (let ([arg+*
                    (for/list ([a (in-syntax #'(arg* ...))]
                               [t (in-list type*)])
                      (if t (quasisyntax/loc #'f (ann #,a #,t)) a))])
        (quasisyntax/loc #'f
          (format template #,@arg+*))))]
   [(f tmp arg* ...)
    (syntax/loc #'f (format tmp arg* ...))]))

(define-syntax-rule (printf: arg* ...)
  (display (format: arg* ...)))

;; -----------------------------------------------------------------------------

;; Count the number of format escapes in a string.
;; Returns a list of optional types (to be spliced into the source code)
;; Example: If result is '(#f Integer), then
;; - Expect 2 arguments to format string
;; - First argument has no constraints, second must be an Integer
;; (: count-format-escapes (->* [String] [#:src (U #f Syntax)] (Listof (U #f Syntax))))
(define-for-syntax (template->type* str #:src [stx #f])
  (define last-index (- (string-length str) 1))
  (let loop ([i 0] [acc '()])
    (cond
     [(>= i last-index)
      (reverse acc)]
     [(eq? #\~ (string-ref str i))
      ;; From fprintf docs
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
      (loop (+ i 1) acc)])))

