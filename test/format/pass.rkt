#lang typed/racket/base

(require trivial/format)

(module+ test
  (printf "Testing statically-checked formatting:\n~a\n"
    (make-string 10 #\=))

  (define-syntax-rule (test-case doc template arg* ...)
    (begin
      (display "TEST ")
      (displayln doc)
      (display "  [printf] ")
      (printf: template arg* ...)
      (newline)
      (display "  [format] ")
      (display (format: template arg* ...))
      (newline)
      (newline)))

  (define H "hello")

  (test-case "without arguments"
    "success")
  (test-case "one '~a' arg"
    "~a" H)
  (test-case "one '~A' arg"
    "~A" H)
  (test-case "one '~b' arg"
    "~b" 9)
  (test-case "one '~B' arg"
    "~B" 9)
  (test-case "one '~c' arg"
    "~c" #\Y)
  (test-case "one '~C' arg"
    "~C" #\Y)
  (test-case "one '~e' arg"
    "~e" H)
  (test-case "one '~E' arg"
    "~E" H)
  (test-case "one '~o' arg"
    "~o" 9)
  (test-case "one '~O' arg"
    "~O" 9)
  (test-case "one '~s' arg"
    "~s" H)
  (test-case "one '~S' arg"
    "~S" H)
  (test-case "one '~v' arg"
    "~v" H)
  (test-case "one '~V' arg"
    "~V" H)
  (test-case "one '~x' arg"
    "~x" 12)
  (test-case "one '~X' arg"
    "~X" 12)
  (test-case "~<whitespace>"
    "hello ~        \n   world")

  (parameterize ([error-print-width 4])
    (test-case "two 'display' args, second truncated"
      "arg1 = ~a, arg2 = ~.a" "hello" "world"))

  (test-case "string with newline"
    "begin... ~n                ...end")
)
