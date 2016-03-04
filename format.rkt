#lang typed/racket/base

;; Statically-checked format strings

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
  (for-syntax
    trivial/private/common
    typed/racket/base
    syntax/parse
    racket/sequence))

;; =============================================================================

(begin-for-syntax
(define-syntax-class/predicate string/expanded string?)
(define-syntax-class string/format
  #:attributes (expanded type*)
  (pattern e:string/expanded
   #:with maybe-type* (template->type* #'e.expanded)
   #:when (syntax-e #'maybe-type*)
   #:attr type* #'maybe-type*
   #:attr expanded #'e.expanded))
)

(define-syntax format:
  (syntax-parser
   [(f fmt:string/format arg* ...)
    ;; 1. Parse expected types from the template
    #:when (let ([num-expected (length (syntax-e #'fmt.type*))]
                 [num-given (length (syntax-e #'(arg* ...)))])
             (unless (= num-expected num-given)
               (apply raise-arity-error
                 (syntax-e #'f)
                 num-expected
                 (map syntax->datum (syntax-e #'(arg* ...))))))
    ;; 2. If any types left obligations, use `ann` to typecheck the args
    #:with (arg+* ...)
      (for/list ([a (in-syntax #'(arg* ...))]
                 [t (in-syntax #'fmt.type*)])
        (if (syntax-e t) (quasisyntax/loc #'f (ann #,a #,t)) a))
    (syntax/loc #'f (format 'fmt.expanded arg+* ...))]
   [f:id
    (syntax/loc #'f format)]
   [(f tmp arg* ...)
    (syntax/loc #'f (format tmp arg* ...))]))

;; Short for `(displayln (format: ...))`
(define-syntax printf:
  (syntax-parser
   [f:id
    (syntax/loc #'f printf)]
   [(f arg* ...)
    (syntax/loc #'f (display (format: arg* ...)))]))

;; -----------------------------------------------------------------------------

;; Count the number of format escapes in a string.
;; Returns a list of optional types (to be spliced into the source code).
;;   Example: If result is '(#f Integer), then
;;   - The format string expects 2 arguments
;;   - First argument has no type constraints, second must be an Integer
;; (: template->type (->* [Syntax] (Listof (U #f Syntax))))
(define-for-syntax (template->type* stx)
  (define str (syntax-e stx))
  (unless (string? str) (error 'template->type "Internal error: ~a" str))
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
      (loop (+ i 1) acc)])))
