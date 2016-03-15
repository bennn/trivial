#lang racket/base

;; Run benchmarks

(require
  (for-syntax racket/base syntax/parse)
  glob
  (only-in racket/list last)
  (only-in racket/format ~r)
  racket/runtime-path
  (only-in racket/file file->value)
  racket/port
  racket/string
  racket/system
  math/statistics
)

;; =============================================================================



(define-runtime-path HERE ".")
(define PRE "pre")
(define POST "post")
(define NUM-ITERS 15)
(define VERSION "6.4")
(define RACO (string-append "raco" VERSION))
(define RACKET (string-append "racket" VERSION))

(define-syntax-rule (log msg arg* ...)
  (begin
    (display "[TEST] ")
    (printf msg arg* ...)
    (newline)))

(define-syntax (confirm stx)
  (syntax-parse stx
   [(_ e)
    #:with msg #`(format "Execute ~a ? (Y/N) " #,(syntax->datum #'e))
    #'(let loop ([response (read-line)])
        (case (string-upcase response)
         (("Y" "YE" "YES")
          e)
         (("N" "NO")
          (printf "Goodbye\n"))
         (else
          (display msg))))]))

(define-syntax-rule (system/assert tmp arg* ...)
  (let ([cmd (format tmp arg* ...)])
    (unless (system cmd)
      (raise-user-error 'system "command failed: ~a" cmd))))

;; -----------------------------------------------------------------------------

(define (dir->pre/post dir)
  (values
    (string-append dir "/" PRE)
    (string-append dir "/" POST)))

(define (rnd n)
  (string->number (~r n #:precision 2)))

(define (mean+stddev x*)
  (define m (mean x*))
  (cons (rnd m) (rnd (stddev/mean m x*))))

(define (real-time f)
  (define-values (_res _cpu real _gc)
    (time-apply f '()))
  real)

(define (compile/time)
  (system/assert "rm -rf compiled")
  (real-time (lambda () (system/assert "~a make main.rkt" RACO))))

(define (run/time)
  (real-time (lambda () (system/assert "~a main.rkt" RACKET))))

(define (benchmark dir f)
  (mean+stddev
    (parameterize ([current-directory dir])
      (for/sum ([_i (in-range NUM-ITERS)])
        (f)))))

(define (test dir f)
  (define-values (pre post) (dir->pre/post dir))
  (values (benchmark pre f)
          (benchmark post f)))

(define (test-compile d)
  (define-values (a b) (test d compile/time))
  (values a b))

(define (test-run d)
  (define-values (a b) (test d run/time))
  (values a b))

(define (sloccount d #:cache? [cache? #f])
  (system (format "sloccount --details ~a ~a"
            (if cache? "--cached" "")
            d)))

(define (loc-one d)
  ;; First compute dummy output
  (parameterize ([current-output-port (open-output-nowhere)])
    (unless (sloccount d)
      (raise-user-error 'test-loc "Failed to get LOC for '~a'" d)))
  (define row*
    (with-output-to-string
      (lambda () (sloccount d #:cache? #t))))
  (for/sum ([line (in-list (string-split row* "\n"))])
    (string->number (car (string-split line)))))

(define (test-loc d)
  (define-values (pre post) (dir->pre/post d))
  (values (loc-one pre)
          (loc-one post)))

(define (bytes-one d)
  (define ls-l*
    (with-output-to-string
      (lambda () (system/assert "ls -l ~a/compiled/*.zo" d))))
  ;; bytes is 5th
  (for/sum ([line (in-list (string-split ls-l* "\n"))])
    (string->number (car (cddddr (string-split line))))))

(define (test-bytes d)
  (define-values (pre post) (dir->pre/post d))
  (values (bytes-one pre)
          (bytes-one post)))

(define (test-diff d)
  ;; TODO get actual differences, not just diff size
  (define-values (pre post) (dir->pre/post d))
  (define diff*
    (with-output-to-string
      (lambda () (system (format "diff --exclude=compiled -U0 -r ~a ~a" pre post)))))
  (for/fold ([add 0]
             [rem 0])
            ([line (in-list (string-split diff* "\n"))])
    (cond
     [(and (string-prefix? line "+")
           (not (string-prefix? line "+++ ")))
      (values (+ 1 add) rem)]
     [(and (string-prefix? line "-")
           (not (string-prefix? line "--- ")))
      (values add (+ 1 rem))]
     [else
      (values add rem)])))

(define (run-tests dir*)
  (printf "(   ;; DIR | compile-pre | compile-post | run-pre | run-post | loc-pre | loc-post | bytes-pre | bytes-post | diff+ | diff- \n")
  (printf "    ;;   all times in milliseconds\n")
  (printf "    ;;   'loc' is lines of code, generated using 'SLOCCount' by David A. Wheeler.\n")
  (for ([d (in-list dir*)])
    (define-values (c-pre c-post) (test-compile d))
    (define-values (r-pre r-post) (test-run d))
    (define-values (l-pre l-post) (test-loc d))
    (define-values (b-pre b-post) (test-bytes d))
    (define-values (diff+ diff-) (test-diff d))
    ;(writeln (list d b-pre b-post diff))
    (writeln (list (string->symbol d)
                   (list c-pre c-post)
                   (list r-pre r-post)
                   (list l-pre l-post)
                   (list b-pre b-post)
                   (list diff+ diff-)))
    (void))
  (printf ")\n"))

(define (filenames d)
  (for/list ([f (in-glob (string-append d "/*.rkt"))])
    (last (string-split f "/"))))

(define (is-not-test-dir? dir*)
  (define-values (pre-dir post-dir) (dir->pre/post dir*))
  (cond
   [(not (directory-exists? dir*))
    "could not find directory"]
   [(not (directory-exists? pre-dir))
    "'<dir>/pre' does not exist"]
   [(not (directory-exists? post-dir))
    "'<dir>/post' does not exist"]
   [(not (equal? (filenames pre-dir) (filenames post-dir)))
    "pre/post directories have unequal files"]
   [else
    #f]))

(define (log-bad-dir d reason)
  (log "skipping directory '~a' because ~a" d reason))

(define (filter-tests dir*)
  (for/list ([d (in-list dir*)]
             #:when (let ([bad? (is-not-test-dir? d)])
                      (if bad? (and (log-bad-dir d bad?) #f) #t)))
    d))

(define (run-all-tests)
  (run-tests (filter-tests (directory-list HERE))))

;; -----------------------------------------------------------------------------
;; -- Extracting data

(define (ctimes d)
  (define c (cadr d))
  (cons (caar c)
        (car (cadr c))))

(define (rtimes d)
  (define r (caddr d))
  (cons (caar r)
        (car (cadr r))))

(define (bytes d)
  (define b (car (cdddr d)))
  (cons (car b) (cadr b)))

(define (diff x)
  (define b4 (car x))
  (define after (cdr x))
  (rnd
    (* 100 (/ (- after b4)
            b4))))

(define (extract)
  (define x* (file->value "output-albany.rktd"))
  (for ([d (in-list x*)])
    (printf "~a  ~a  ~a  ~a\n" (car d) (diff (ctimes d)) (diff (rtimes d)) (diff (bytes d)))))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:args DIR*
   (if (null? DIR*)
     (confirm (run-all-tests))
     (run-tests (filter-tests DIR*)))))
