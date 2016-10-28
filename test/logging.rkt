#lang racket/base

(module+ test
  (require rackunit trivial trivial/private/test-common)

  (test-case "integer"
    (check-trivial-log-sequence
      (begin (+ 9 9)
             (- 9 9)
             (* 9 9)
             (/ 9 9)
             (expt 9 9)
             (expt (current-seconds) 9)
             (quotient 9 9))
      '((CHECK+ +)
        (CHECK+ -)
        (CHECK+ *)
        (CHECK+ /)
        (CHECK+ expt)
        (CHECK+ expt)
        (CHECK+ quotient)))

    (check-trivial-log-sequence
      (begin (+ (current-seconds) 9)
             (- (current-seconds) 9)
             (* (current-seconds) 9)
             (/ (current-seconds) 9)
             (expt 9 (current-seconds))
             (quotient (current-seconds) 9))
      '((CHECK- +)
        (CHECK- -)
        (CHECK- *)
        (CHECK- /)
        (CHECK- expt)
        (CHECK- quotient))))

  (test-case "regexp"
    (check-trivial-log-sequence
      (begin (regexp "()")
             (pregexp "()")
             (byte-regexp "()")
             (byte-pregexp "()"))
      '((INFER+ regexp)
        (INFER+ pregexp)
        (INFER+ byte-regexp)
        (INFER+ byte-pregexp)))

    (check-trivial-log-sequence
      (begin (regexp-match "()" "yo")
             (regexp-match (symbol->string 'abc) "yo"))
      '((CHECK+ regexp-match)
        (CHECK- regexp-match))))

  (test-case "vector"

    (check-trivial-log-sequence
      (begin (vector 0)
             (build-vector 0 (λ (x) x))
             (make-vector 0))
      '((INFER+ vector)
        (INFER+ build-vector)
        (INFER+ make-vector)))

    (check-trivial-log-sequence
      (begin (vector-length (vector 1 2 3))
               (vector-length (current-command-line-arguments)))
      '((INFER+ vector)
        (CHECK+ vector-length)
        (CHECK- vector-length)))

    (check-trivial-log-sequence
      (begin (vector-ref (vector 1 2 3) 0)
             (vector-ref (current-command-line-arguments) 0))
      '((INFER+ vector)
        (CHECK+ vector-ref)
        (CHECK- vector-ref)))

    (check-trivial-log-sequence
      (begin (vector-set! (vector 0) 0 1)
             (vector-set! (current-command-line-arguments) 0 0))
      '((INFER+ vector)
        (CHECK+ vector-set!)
        (CHECK- vector-set!)))

    (check-trivial-log-sequence
      (begin (vector-map values (vector 1 2 3))
             (vector-map values (current-command-line-arguments)))
      '((INFER+ vector)
        (CHECK+ vector-map)
          (INFER+ vector)
          (CHECK+ vector-ref)
          (CHECK+ vector-ref)
          (CHECK+ vector-ref)
        (CHECK- vector-map)))

    (check-trivial-log-sequence
      (begin (vector-map! values (vector 1 2 3))
             (vector-map! values (current-command-line-arguments)))
      '((INFER+ vector)
        (CHECK+ vector-map!)
        (CHECK- vector-map!)))

    (check-trivial-log-sequence
      (begin (vector-append #(0) #(1))
             (vector-append (current-command-line-arguments) #(1)))
      '((CHECK+ vector-append)
         (INFER+ vector)
         (CHECK+ vector-ref)
         (CHECK+ vector-ref)
        (CHECK- vector-append)))

    (check-trivial-log-sequence
      (begin (vector->list #(0 0))
             (vector->list (current-command-line-arguments)))
      '((CHECK+ vector->list)
          (CHECK+ vector-ref)
          (CHECK+ vector-ref)
        (CHECK- vector->list)))

    (check-trivial-log-sequence
      (begin (vector->immutable-vector #(1 2 3))
             (vector->immutable-vector (current-command-line-arguments)))
      '((CHECK+ vector->immutable-vector)
        (CHECK- vector->immutable-vector)))

    (check-trivial-log-sequence
      (begin (vector-fill! #(1 2 3) 0)
             (vector-fill! (current-command-line-arguments) 0))
      '((CHECK+ vector-fill!)
        (CHECK- vector-fill!)))

    (check-trivial-log-sequence
      (begin (vector-take #(3) 1)
             (vector-take (current-command-line-arguments) 0))
      '((CHECK+ vector-take)
          (CHECK- +)
          (INFER+ build-vector)
        (CHECK- vector-take)))

    (check-trivial-log-sequence
      (begin (vector-take-right #(9 3) 1)
             (vector-take-right (current-command-line-arguments) 0))
      '((CHECK+ vector-take-right)
          (CHECK- +)
          (INFER+ build-vector)
        (CHECK- vector-take-right)))

    (check-trivial-log-sequence
      (begin (vector-drop #(3) 1)
             (vector-drop (current-command-line-arguments) 0))
      '((CHECK+ vector-drop)
          (CHECK- +)
          (INFER+ build-vector)
        (CHECK- vector-drop)))

    (check-trivial-log-sequence
      (begin (vector-drop-right #(3) 1)
             (vector-drop-right (current-command-line-arguments) 0))
      '((CHECK+ vector-drop-right)
          (CHECK- +)
          (INFER+ build-vector)
        (CHECK- vector-drop-right))))
)
