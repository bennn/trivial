#lang typed/racket/base

;(module+ test
  (require
    trivial/vector
    trivial/private/db)
  (define-type Id Natural)
  (define-schema: M '((word
                       [(id   . Id)
                        (word . String)])
                      (word_syllables
                       [(word      . Id)
                        (syllables . Id)])))
  (define-connection: c (postgresql-connect: M #:user "ben" #:database "ipoe"))

  (define r0 (query-row: c "SELECT * FROM word LIMIT 1"))
  (ann r0 (Vector Id String))

  (define r1 (query-row: c "SELECT * FROM word WHERE word.word = \"blossom\""))
  (ann r1 (Vector Id String))

  (define r2 (query-row: c "SELECT * FROM word WHERE word.word = $1" "blossom"))
  (ann r2 (Vector Id String))

  (define-vector: r3 (query-row: c "SELECT id FROM word WHERE word.id = 1"))
  (ann r3 (Vector Id))

  ;(vector-ref: r3 2)
  ;(vector-ref: (vector-map: add1 r3) 3) ;; Yes

  ;(query-row: c "SELECT * FROM word WHERE word.word = $1 word.id = $2" "blossom")
  ;(query-row: c "SELECT * FROM word WHERE word.word = $1 word.id = $4" "blossom" 2)

