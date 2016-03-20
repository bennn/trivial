#lang typed/racket/base

;; Example using the db library in Typed Racket
;; - require/typed gives specific type
;; - rename-in allows multiple specific types in a file (just need to deal with renamings)

;; -----------------------------------------------------------------------------

(require/typed db
  (#:opaque Connection connection?)
  (postgresql-connect (->* () (#:user String #:database String) Connection)))

(require/typed (prefix-in w: db)
  (w:query-row (-> Connection String Any * (Vector Natural String))))

(require/typed (prefix-in s: db)
  (s:query-row (-> Connection String Any * (Vector Natural Natural))))

;; -----------------------------------------------------------------------------

(define c (postgresql-connect #:user "ben" #:database "ipoe"))
(define r0 (w:query-row c "SELECT * FROM word WHERE word.word=$1" "blossoms"))
(define r1 (s:query-row c "SELECT * FROM word_syllables WHERE word_syllables.word=$1" (vector-ref r0 0)))
(displayln r0)
(displayln r1)
