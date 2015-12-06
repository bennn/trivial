#lang typed/racket/base

(provide
  format!
  ;; (-> (x:String) Any *N Void)
  ;; Takes 1 required string argument and N additional arguments,
  ;;  where N is the number of format sequences in the string.
  ;;
  ;; If the string is a literal, raises a compile-time arity error if
  ;;  the given number of arguments does not match the format string.
  ;;
  ;; If the string is not a literal, arity-checking happens at runtime.

  printf!
  ;; (-> (x:String) Any *N Void)
  ;; Similar to `format`, but displays the formatted string to `current-output-port`.
)

(require
  (for-syntax typed/racket/base syntax/parse racket/sequence))

;; =============================================================================

(define-syntax format!
  (syntax-parser
   [(f template:str arg* ...)
    (let ([num-expected (count-format-escapes (syntax-e #'template))]
          [num-given (for/sum ([a (in-syntax #'(arg* ...))]) 1)])
      (unless (= num-expected num-given)
        (raise-arity-error
          (syntax-e #'f)
          num-expected
          (for/list ([a (in-syntax #'(arg* ...))]) (syntax->datum a))))
      (syntax/loc #'f (format template arg* ...)))]
   [(f tmp arg* ...)
    (syntax/loc #'f (format tmp arg* ...))]))

(define-syntax-rule (printf! arg* ...)
  (display (format! arg* ...)))

;; Count the number of format escapes in a string.
(define-for-syntax (count-format-escapes str)
  (define last-index (- (string-length str) 1))
  (let loop ([i 0] [acc 0])
    (cond
     [(>= i last-index)
      acc]
     [(eq? #\~ (string-ref str i))
      ;; From fprintf docs
      (case (string-ref str (+ i 1))
       [(#\n #\% #\a #\A #\v #\V #\e #\E #\c #\C #\b #\B #\o #\O #\x #\X #\space #\tab #\newline)
        (loop (+ i 2) (+ 1 acc))]
       [(#\.)
        (if (and (< (+ 1 i) last-index)
                 (memq (string-ref str (+ i 2)) '(#\a #\A #\s #\S #\v #\V)))
          (loop (+ i 3) (+ 1 acc))
          (loop (+ i 3) acc))]
       [else
        (loop (+ i 2) acc)])]
     [else
      (loop (+ i 1) acc)])))

;; =============================================================================

(module+ test (begin-for-syntax

  (require
    rackunit)

  ;; --- count-format-escapes
  (define-syntax-rule (test-escapes [str n] ...)
    (begin
      (check-equal? (count-format-escapes str) n) ...))

  (test-escapes
   ["" 0]
   ["hello" 0]
   ["world" 0]
   ["~~ nope" 0]
   ["~" 0]
   ["\\~~" 0]
   ["~~~~" 0]
   ["hey ~.x you" 0]
   ;; --
   ["hello, ~a" 1]
   ["~a ~b ~c" 3]
   ["~\n ~%" 2]
   ["~a ~A ~v ~V ~e ~E ~c ~C ~b ~B ~o ~O ~x ~X" 14]
   ["~ " 1])
))

