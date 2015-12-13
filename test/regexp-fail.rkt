#lang racket/base

;; TODO why can't I catch errors for (ann ... (List String))? WhydoI need #f?

(define TEST-CASE* (syntax->list #'(
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: "hi" "hi")
      (U #f (List String String String))))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: #rx"(h)(i)" "hi")
      (U #f (List String))))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: #px"(?<=h)(?=i)" "hi")
      (U #f (List String String String))))
  (module t typed/racket/base (require trivial/regexp)
    ;;bg; ill-typed in untyped Racket
    (byte-regexp: #rx#"yolo"))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: #rx#"hi" "hi")
      (U #f (List String String))))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: #px#"hi" "hi")
      (U #f (List Bytes Bytes))))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (regexp-match: (regexp "he") "hellooo")
      (U #f (List String))))
  (module t typed/racket/base (require trivial/regexp)
    (ann
      (let ()
        (define-regexp: rx (regexp "he(l*)(o*)"))
        (regexp-match: rx "hellooo"))
      (U #f (List String String String))))
  (module t typed/racket/base (require trivial/regexp)
    ;; `define` doesn't propagate group information
    (ann
      (let ()
        (define rx "he(l*)(o*)")
        (regexp-match: rx "helloooooooo"))
      (U #f (List String String String))))
)))

(module+ test
  (require
    rackunit)

  (define regexp-eval
    (let ([regexp-ns (make-base-namespace)])
      (lambda (stx)
        (lambda () ;; For `check-exn`
          (eval-syntax stx regexp-ns)))))

  (for ([rkt (in-list TEST-CASE*)])
    (check-exn #rx"Type Checker"
      (regexp-eval rkt)))

)
