#lang racket/base
(require trivial/private/test-common
  (only-in typed/racket/base
    with-handlers raise lambda : -> Any List String Natural Integer Vector))

;; === HOLY BOILERPLATE BATMAN

(module+ test (test-compile-error
  #:require trivial/private/db
  #:exn #rx"query-row::|Type Checker"

  ;; ===========================================================================
  ;; === TEST reference missing table
  (let ([fish-sql "CREATE TABLE fish ( id serial PRIMARY KEY, name text UNIQUE NOT NULL, weight int NOT NULL);"]
        [cube-sql "CREATE TABLE cube ( id serial PRIMARY KEY, length smallint NOT NULL, width integer NOT NULL, height bigint NOT NULL);"])
    (let-schema: ([schema '((fish ((id . Natural) (name . String) (weight . Integer))) (cube ((id . Natural) (length . Integer) (width . Integer) (height . Integer))))])
      (let-connection: ([conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test")])
        (let ([with-transaction (lambda ([thunk : (-> Any)])
                                  (define maybe-exn
                                    (with-handlers ([exn? (lambda ([e : exn]) e)])
                                      (start-transaction conn)
                                      (thunk)
                                      #f))
                                  (rollback-transaction conn)
                                  (if maybe-exn (raise maybe-exn) (void)))]
              [insert-fish (lambda ([x : (List String Natural)])
                             (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);" (car x) (cadr x)))]
              [insert-cube (lambda ([x : (List Integer Integer Integer)])
                             (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);" (car x) (cadr x) (caddr x)))])
          (query-exec conn fish-sql)
          (query-exec conn cube-sql)
          (define f1 '("Marlin" 8))
          (define c1 '(2 4 8))
          (insert-fish f1)
          (insert-cube c1)
          ;; -------------------------------------------------------------------
          (query-row: conn "SELECT * FROM fake_table")))))

  ;; ===========================================================================
  ;; === TEST missing row
  (let ([fish-sql "CREATE TABLE fish ( id serial PRIMARY KEY, name text UNIQUE NOT NULL, weight int NOT NULL);"]
        [cube-sql "CREATE TABLE cube ( id serial PRIMARY KEY, length smallint NOT NULL, width integer NOT NULL, height bigint NOT NULL);"])
    (let-schema: ([schema '((fish ((id . Natural) (name . String) (weight . Integer))) (cube ((id . Natural) (length . Integer) (width . Integer) (height . Integer))))])
      (let-connection: ([conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test")])
        (let ([with-transaction (lambda ([thunk : (-> Any)])
                                  (define maybe-exn
                                    (with-handlers ([exn? (lambda ([e : exn]) e)])
                                      (start-transaction conn)
                                      (thunk)
                                      #f))
                                  (rollback-transaction conn)
                                  (if maybe-exn (raise maybe-exn) (void)))]
              [insert-fish (lambda ([x : (List String Natural)])
                             (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);" (car x) (cadr x)))]
              [insert-cube (lambda ([x : (List Integer Integer Integer)])
                             (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);" (car x) (cadr x) (caddr x)))])
          (query-exec conn fish-sql)
          (query-exec conn cube-sql)
          (define f1 '("Marlin" 8))
          (define c1 '(2 4 8))
          (insert-fish f1)
          (insert-cube c1)
          ;; -------------------------------------------------------------------
          (query-row: conn "SELECT * FROM fish where fish.fry = 1")))))

  ;; ===========================================================================
  ;; === TEST missing actual parameter
  (let ([fish-sql "CREATE TABLE fish ( id serial PRIMARY KEY, name text UNIQUE NOT NULL, weight int NOT NULL);"]
        [cube-sql "CREATE TABLE cube ( id serial PRIMARY KEY, length smallint NOT NULL, width integer NOT NULL, height bigint NOT NULL);"])
    (let-schema: ([schema '((fish ((id . Natural) (name . String) (weight . Integer))) (cube ((id . Natural) (length . Integer) (width . Integer) (height . Integer))))])
      (let-connection: ([conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test")])
        (let ([with-transaction (lambda ([thunk : (-> Any)])
                                  (define maybe-exn
                                    (with-handlers ([exn? (lambda ([e : exn]) e)])
                                      (start-transaction conn)
                                      (thunk)
                                      #f))
                                  (rollback-transaction conn)
                                  (if maybe-exn (raise maybe-exn) (void)))]
              [insert-fish (lambda ([x : (List String Natural)])
                             (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);" (car x) (cadr x)))]
              [insert-cube (lambda ([x : (List Integer Integer Integer)])
                             (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);" (car x) (cadr x) (caddr x)))])
          (query-exec conn fish-sql)
          (query-exec conn cube-sql)
          (define f1 '("Marlin" 8))
          (define c1 '(2 4 8))
          (insert-fish f1)
          (insert-cube c1)
          ;; -------------------------------------------------------------------
          (query-row: conn "SELECT * FROM fish where fish.name = $1")))))

  ;; ===========================================================================
  ;; === TEST wrong type for actual parameter
  (let ([fish-sql "CREATE TABLE fish ( id serial PRIMARY KEY, name text UNIQUE NOT NULL, weight int NOT NULL);"]
        [cube-sql "CREATE TABLE cube ( id serial PRIMARY KEY, length smallint NOT NULL, width integer NOT NULL, height bigint NOT NULL);"])
    (let-schema: ([schema '((fish ((id . Natural) (name . String) (weight . Integer))) (cube ((id . Natural) (length . Integer) (width . Integer) (height . Integer))))])
      (let-connection: ([conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test")])
        (let ([with-transaction (lambda ([thunk : (-> Any)])
                                  (define maybe-exn
                                    (with-handlers ([exn? (lambda ([e : exn]) e)])
                                      (start-transaction conn)
                                      (thunk)
                                      #f))
                                  (rollback-transaction conn)
                                  (if maybe-exn (raise maybe-exn) (void)))]
              [insert-fish (lambda ([x : (List String Natural)])
                             (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);" (car x) (cadr x)))]
              [insert-cube (lambda ([x : (List Integer Integer Integer)])
                             (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);" (car x) (cadr x) (caddr x)))])
          (query-exec conn fish-sql)
          (query-exec conn cube-sql)
          (define f1 '("Marlin" 8))
          (define c1 '(2 4 8))
          (insert-fish f1)
          (insert-cube c1)
          ;; -------------------------------------------------------------------
          (query-row: conn "SELECT * FROM fish where fish.name = $1" 666)))))

  ;; ===========================================================================
  ;; === TEST skipping query parameter
  (let ([fish-sql "CREATE TABLE fish ( id serial PRIMARY KEY, name text UNIQUE NOT NULL, weight int NOT NULL);"]
        [cube-sql "CREATE TABLE cube ( id serial PRIMARY KEY, length smallint NOT NULL, width integer NOT NULL, height bigint NOT NULL);"])
    (let-schema: ([schema '((fish ((id . Natural) (name . String) (weight . Integer))) (cube ((id . Natural) (length . Integer) (width . Integer) (height . Integer))))])
      (let-connection: ([conn (postgresql-connect: schema #:user "postgres" #:database "travis_ci_test")])
        (let ([with-transaction (lambda ([thunk : (-> Any)])
                                  (define maybe-exn
                                    (with-handlers ([exn? (lambda ([e : exn]) e)])
                                      (start-transaction conn)
                                      (thunk)
                                      #f))
                                  (rollback-transaction conn)
                                  (if maybe-exn (raise maybe-exn) (void)))]
              [insert-fish (lambda ([x : (List String Natural)])
                             (query-exec conn "INSERT INTO fish (name, weight) VALUES ($1, $2);" (car x) (cadr x)))]
              [insert-cube (lambda ([x : (List Integer Integer Integer)])
                             (query-exec conn "INSERT INTO cube (length, width, height) VALUES ($1, $2, $3);" (car x) (cadr x) (caddr x)))])
          (query-exec conn fish-sql)
          (query-exec conn cube-sql)
          (define f1 '("Marlin" 8))
          (define c1 '(2 4 8))
          (insert-fish f1)
          (insert-cube c1)
          ;; -------------------------------------------------------------------
          (query-row: conn "SELECT * FROM fish where fish.name = $3" "Marlin")))))

))
