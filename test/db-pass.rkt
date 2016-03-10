#lang typed/racket/base

;; To run these test:
;; - Install postgres, start server
;; - Create superuser 'postgres'
;; - Create database 'travis_ci_test'
;; Then you can `raco test ...` as usual

(module+ test
  (require
    typed/rackunit
    trivial/vector
    trivial/private/db)

  (require/typed db
    (start-transaction (-> Connection Void))
    (rollback-transaction (-> Connection Void))
    (query-exec (-> Connection String Any * Void)))

  (define-type Id Natural)

  ;; -- create fake database
  (define fish-sql
    (string-append
      "CREATE TABLE fish ( "
      "id serial PRIMARY KEY, "
      "name text UNIQUE NOT NULL, "
      "weight int NOT NULL);"))

  (define cube-sql
    (string-append
      "CREATE TABLE cube ( "
      "id serial PRIMARY KEY, "
      "length smallint NOT NULL, "
      "width integer NOT NULL, "
      "height bigint NOT NULL);"))

  (define-schema: schema
    '((fish ((id . Id) (name . String) (weight . Integer)))
      (cube ((id . Id) (length . Integer) (width . Integer) (height . Integer)))))

  (define-connection: conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test"))

  (struct fish ([name : String] [weight : Integer]))
  (define-type Fish fish)
  (struct cube ([length : Integer] [width : Integer] [height : Integer]))
  (define-type Cube cube)

  (: insert-fish (-> Fish Void))
  (define (insert-fish f)
    (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);"
      (fish-name f) (fish-weight f)))

  (: insert-cube (-> Cube Void))
  (define (insert-cube c)
    (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);"
      (cube-length c) (cube-width c) (cube-height c)))

  (: with-transaction (-> (-> Any) Void))
  (define (with-transaction thunk)
    (begin
      (define maybe-exn
        (with-handlers ([exn? (lambda ([e : exn]) e)])
          (start-transaction conn)
          (thunk)
          #f))
      (rollback-transaction conn)
      (if maybe-exn (raise maybe-exn) (void))))

  (with-transaction (lambda ()
  ;; ---------------------------------------------------------------------------
  ;; Insert some things
    (query-exec conn fish-sql)
    (query-exec conn cube-sql)

    (define f1 (fish "Swordfish" 432))
    (define f2 (fish "Tuna" 9999))
    (define f3 (fish "Dorado" 12))
    (insert-fish f1)
    (insert-fish f2)
    (insert-fish f3)

    (define c1 (cube 1 1 1))
    (define c2 (cube 88 88 132))
    (define c3 (cube 1 20 300))
    (insert-cube c1)
    (insert-cube c2)
    (insert-cube c3)

    ;; ---------------------------------------------------------------------------

    (check-equal?
      (ann (query-row: conn "SELECT * FROM fish LIMIT 1") (Vector Id String Integer))
      (vector 1 (fish-name f1) (fish-weight f1)))

    (check-equal?
      (ann (query-row: conn "SELECT * FROM cube LIMIT 1") (Vector Id Integer Integer Integer))
      (vector 1 (cube-length c1) (cube-width c1) (cube-height c1)))

    (check-equal?
      (ann (query-row: conn "SELECT * FROM fish WHERE fish.name = 'Tuna'") (Vector Id String Integer))
      (vector 2 (fish-name f2) (fish-weight f2)))

    (check-equal?
      (ann (query-row: conn "SELECT * FROM cube WHERE cube.width = $1" 20) (Vector Id Integer Integer Integer))
      (vector 3 (cube-length c3) (cube-width c3) (cube-height c3)))

    (let-vector: ([v (query-row: conn "SELECT id FROM fish WHERE fish.id = 2")])
      (check-equal? (vector-ref: v 0) 2)
      (check-equal?
        (ann (vector-length: v) One)
        1))

  ;(query-row: c "SELECT * FROM word WHERE word.word = $1 word.id = $2" "blossom")
  ;(query-row: c "SELECT * FROM word WHERE word.word = $1 word.id = $4" "blossom" 2)
)))
