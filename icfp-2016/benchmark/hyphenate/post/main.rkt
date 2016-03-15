#lang typed/racket/base
(require trivial/no-colon)
(require "hyphenate.rkt" typed/rackunit)

(define (main)
  (check-equal? (hyphenate "edges") "edges") ;; word without matching patterns
  (check-equal? (hyphenate "polymorphism") "poly\u00ADmor\u00ADphism")
  (check-equal? (hyphenate "POLYmorPHISM") "POLY\u00ADmor\u00ADPHISM")
  (check-equal? (hyphenate "polymorphism" #:min-length 100) "polymorphism")
  (check-equal? (hyphenate "ugly" #:min-length 1) "ug\u00ADly")
  (check-equal? (unhyphenate "poly\u00ADmor\u00ADphism") "polymorphism")
  (check-equal? (hyphenate "polymorphism" #\-) "poly-mor-phism")
  (check-equal? (hyphenate "polymorphism" "foo") "polyfoomorfoophism")
  (check-equal? (unhyphenate "polyfoomorfoophism" "foo") "polymorphism")
  (check-equal? (hyphenate "circular polymorphism squandering") "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism squan\u00ADder\u00ADing")
  (check-equal? (hyphenate "present project") "present project") ; exception words
  
  (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 5 #:min-right-length 5) "polymor-phism")
  (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 3 #:min-right-length 7) "poly-morphism")
  (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 7 #:min-right-length 7) "polymorphism")
  (check-equal? (hyphenate "polymorphism" #\* #:exceptions '("polymo-rphism")) "polymo*rphism")
  
  (check-equal? (hyphenate "formidable" #\-) "for-mi-da-ble")

  (with-input-from-file "../base/common-words.rktd"
    (lambda ()
      (for ([word (in-lines)])
        (hyphenate word))))
)

(time (main))
