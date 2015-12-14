#lang typed/racket/base

;; Successful use of `format:`

(module+ test

  (require
    trivial/format
    typed/rackunit
    racket/port)

  (define-syntax-rule (test-format: [template arg* ...] expect)
    (begin
      (check-equal?
        (format: template arg* ...)
        expect)
      (check-equal?
        (with-output-to-string (lambda () (printf: template arg* ...)))
        expect)))

  (test-format: ["success"] "success")
  (test-format: ["~a" "hi"] "hi")
  (test-format: ["~A" "hi"] "hi")
  (test-format: ["~b" 9] "1001")
  (test-format: ["~B" 9] "1001")
  (test-format: ["~c" #\Y] "Y")
  (test-format: ["~C" #\Y] "Y")
  (test-format: ["~e" "hi"] "\"hi\"")
  (test-format: ["~E" "hi"] "\"hi\"")
  (test-format: ["~o" 9] "11")
  (test-format: ["~O" 9] "11")
  (test-format: ["~s" "hi"] "\"hi\"")
  (test-format: ["~S" "hi"] "\"hi\"")
  (test-format: ["~v" "hi"] "\"hi\"")
  (test-format: ["~V" "hi"] "\"hi\"")
  (test-format: ["~x" 12] "c")
  (test-format: ["~X" 12] "c")
  (test-format: ["hello ~   \n world"] "hello world")
  (test-format: ["begin...~n...end"] "begin...\n...end")

  (parameterize ([error-print-width 4])
    (test-format: ["~a ~.a" "hello" "world"] "hello w..."))

  ;; Higher-order use: should work, but lose typechecking
  (check-equal?
    ((lambda ([f : (-> String Any String)] [x : Any]) (f "hello ~a" x))
     format: 'world)
    "hello world")

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> String Any Void)])
         (f "~b" "not-a-number"))
       printf:)))

)
