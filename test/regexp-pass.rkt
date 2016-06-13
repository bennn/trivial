#lang typed/racket/base

;; Well-typed use of regexp:

(module+ test

  (require
    trivial/regexp
    typed/rackunit)

  ;; -- TODO (handle regexp . format)
  ; (define re:normal-name (regexp (format "~a*<([^>]*)>~a*" blank blank)))

  ;; -- regexps, from the world

  (let ([str 

  (let () ;; -- from klocker? anyway the unicode will segfault `unsafe-string-ref`
    (check-equal?
      (ann (regexp-match: #rx"⇒" "yolo") (U #f (List String)))
      #f))

  (let ([str "dont care"]) ;; from `tests/racket/contract/multi-file.rkt`
    (check-equal?
      (ann
        (regexp-match: #rx"[/\\]([-a-z0-9.]*)[^/\\]*$" str)
        (U #f (List String String)))
      #f))

  (let ([l "dont care"]) ;; from `morse-code-table.rkt`
    (check-equal?
      (ann
        (regexp-match: #rx"[]] [^]]" l)
        (U #f (List String)))
      #f)
    (check-equal?
      (ann
        (regexp-match: #px"^\\| \\[\\[[^]]*\\]\\] \\[([^]]*)\\] \\|\\| '''([^']*)'''" l)
        (U #f (List String String String)))
      #f))

  (let ([str "1cm"]) ;; from html-render.rkt
    (check-equal?
      (ann (regexp-match: #rx"^([+-]?[0-9]*\\.?([0-9]+)?)(em|ex|px|in|cm|mm|pt|pc|%|)$" str)
           (U #f (List String String (U #f String) String)))
      (list str "1" #f "cm")))

  (let ([expr "x+y*x"]) ;; from racket-doc/guide/scribblings/arith.rkt
    (check-equal?
      (ann (regexp-match: #px"^([a-z]|[0-9]+)(?:[-+*/]([a-z]|[0-9]+))*(?![-+*/])" expr)
           (U #f (List String String String)))
      (list expr "x" "x")))

  (let ([str "(this and that!)"]) ;; from load-one.rkt
    (check-equal?
      (ann (regexp-match: #rx"^[(].*[)]$" str) (U #f (List String)))
      (list str)))

  (let ()
    (check-true (and (regexp: "^(\r|\n|(\r\n))") #t)))

  (let ([str "Pete would gain 4."]) ;; from Matthew Butterick's Advent of Code solutions
    (check-equal?
      (ann (regexp-match: #px"^(.*?) would (gain|lose) (\\d+)\\.$"  str)
           (U #f (List String String String String)))
      (list str "Pete" "gain" "4")))

  (let ([str "| yo"]) ;; from John Clement's morse-code-trainer
    (check-equal?
      (ann (regexp-match: #px"hey [|] (yo)" str)
           (U #f (List String String)))
      #f))

  (let ([l "0  afAF09   AF09af   ABSD_asdf    ="]) ;; from racket/src/worksp/gendef.rkt
    (define m : (U #f (List String String String))
      (regexp-match:
         #rx"([0-9]+) +(?:[0-9A-Fa-f]+) +(?:[0-9A-Fa-f]+) +([_A-Za-z][_A-Za-z0-9]*) +="
         l))
    (check-equal? m (list l "0" "ABSD_asdf")))

  ;; -- regexp-match:
  (check-equal?
    (ann
      (regexp-match: "hello" "hello world")
      (U #f (List String)))
    '("hello"))

  (check-equal?
    (ann
      (regexp-match: "hello" "world")
      (U #f (List String)))
    #f)

  (check-equal?
    (ann
      (regexp-match: "he(l*)o" "hellllloooo")
      (U #f (List String String)))
    '("helllllo" "lllll"))

  (check-equal?
    (ann
      (regexp-match: #rx"he(l*)o" "helllooo")
      (U #f (List String String)))
    '("helllo" "lll"))

  (check-equal?
    (ann
      (regexp-match: #rx"h(e(l*))(o)" "helllooo")
      (U #f (List String String String String)))
    '("helllo" "elll" "lll" "o"))

  (check-equal?
    (ann
      (regexp-match: #px"h(e(l*))(o)" "helllooo")
      (U #f (List String String String String)))
    '("helllo" "elll" "lll" "o"))

  (check-equal?
    (ann
      (regexp-match: #rx#"h(e(l*))(o)" "helllooo")
      (U #f (List Bytes Bytes Bytes Bytes)))
    '(#"helllo" #"elll" #"lll" #"o"))

  (check-equal?
    (ann
      (regexp-match: #px#"h(e(l*))(o)" "helllooo")
      (U #f (List Bytes Bytes Bytes Bytes)))
    '(#"helllo" #"elll" #"lll" #"o"))

  ;; -- higher-order
  ;; --- regexp-match:
  (check-equal?
    ((lambda ([f : (-> String String (U #f (Listof (U #f String))))])
      (f "hi" "ahi tuna"))
     regexp-match:)
    '("hi"))

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> String String Any)])
         (f "ah(oy" "ahoy"))
       regexp-match:)))

  ;; --- regexp:
  (check-equal?
    ((lambda ([f : (-> String Regexp)])
      (f "aloha"))
     regexp:)
    (regexp "aloha"))

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> String Regexp)])
         (f "ah(oy"))
       regexp:)))

  ;; --- pregexp:
  (check-equal?
    ((lambda ([f : (-> String PRegexp)])
      (f "aloha"))
     pregexp:)
    (pregexp "aloha"))

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> String PRegexp)])
         (f "ah(oy"))
       pregexp:)))

  ;; --- byte-regexp:
  (check-true ;;bg; bug in 6.3
    (equal?
      ((lambda ([f : (-> Bytes Byte-Regexp)])
         (f #"aloha"))
        byte-regexp:)
      (byte-regexp #"aloha")))

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> Bytes Byte-Regexp)])
         (f #"ah(oy"))
       byte-regexp:)))

  ;; --- byte-pregexp:
  (check-true
    (equal?
      ((lambda ([f : (-> Bytes Byte-PRegexp)])
         (f #"aloha"))
       byte-pregexp:)
      (byte-pregexp #"aloha")))

  (check-exn exn:fail:contract?
    (lambda ()
      ((lambda ([f : (-> Bytes Byte-PRegexp)])
         (f #"ah(oy"))
       byte-pregexp:)))

  ;; -- let-regexp:
  (check-equal?
    (ann
      (let-regexp: ([rx #rx"^y(o+)lo$"])
        (cond
         [(regexp-match: rx "yolo")
          => (lambda ([x* : (List String String)])
            (cadr x*))]
          ;=> cadr]
         [else
          (raise-user-error 'nope)]))
      String)
    "o")

  (check-equal?
    (ann
      (let-regexp: ([rx1 #rx"^y(o+)lo$"]
                    [rx2 #rx"^w(e+)pa$"]
                    [rx3 #rx"^y(e+)rrr$"])
        (cond
         [(regexp-match: rx1 "wepa")
          => cadr]
         [(regexp-match: rx2 "yolo")
          => cadr]
         [(regexp-match: rx3 "yeeeeeerrr")
          => cadr]
         [else
          (raise-user-error 'nope)]))
      String)
    "eeeeee")

  (check-equal?
    (ann
      (let-regexp: ([rx "\\(\\)he(l*)(o*)"])
        (regexp-match: rx "helllooo"))
      (U #f (List String String String)))
    #f)

  ;; -- define-regexp:
  (check-equal?
    (ann
      (let ()
        (define-regexp: rx "\\(\\)he(l*)(o*)")
        (regexp-match: rx "helllooo"))
      (U #f (List String String String)))
    #f)

  (check-equal?
    (ann
      (let ()
        (define-regexp: rx #rx"he(l*)(o*)")
        (regexp-match: rx "helllooo"))
      (U #f (List String String String)))
    '("helllooo" "lll" "ooo"))

  (check-equal?
    (ann
      (let ()
        (define-regexp: rx #rx"h(?=e)(l*)(o*)")
        (regexp-match: rx "hello"))
      (U #f (List String String String)))
    '("h" "" ""))

  (check-equal?
    (ann
      (let ()
        (regexp-match: (regexp: "he(l*)(o*)") "hellooo"))
      (U #f (List String String String)))
    '("hellooo" "ll" "ooo"))

  (check-equal?
    (ann
      (let ()
        (define-regexp: rx (regexp "he(l*)(o*)"))
        (regexp-match: rx "hellooo"))
      (U #f (Listof (U #f String))))
    '("hellooo" "ll" "ooo"))

  (check-equal?
    (ann
      (let ()
        (regexp-match: (pregexp: "he(l*)(o*)") "hellooo"))
      (U #f (List String String String)))
    '("hellooo" "ll" "ooo"))

  (check-equal?
    (ann
      (regexp-match: #rx#"he(l*)(o*)" #"helllooo")
      (U #f (List Bytes Bytes Bytes)))
    '(#"helllooo" #"lll" #"ooo"))

  (check-equal?
    (ann
      (let ()
        (regexp-match: (byte-regexp: #"he(l*)(o*)") "hellooo"))
      (U #f (List Bytes Bytes Bytes)))
    '(#"hellooo" #"ll" #"ooo"))

  (check-equal?
    (ann
      (regexp-match: #px#"he(l*)(o*)" "helllooo")
      (U #f (List Bytes Bytes Bytes)))
    '(#"helllooo" #"lll" #"ooo"))

  (check-equal?
    (ann
      (let ()
        (regexp-match: (byte-pregexp: #"he(l*)(o*)") "hellooo"))
      (U #f (List Bytes Bytes Bytes)))
    '(#"hellooo" #"ll" #"ooo"))

  ;; -- special cases / miscellaneous
  (check-equal?
    (ann
      (regexp-match: "((a)b)" "ab")
      (U #f (List String String String)))
    '("ab" "ab" "a"))

  ;; --- Can't handle |
  (check-equal?
    (ann
      (regexp-match: "this(group)|that" "that")
      (U #f (Listof (U #f String))))
    '("that" #f))

  (let-regexp: ([NODE_REGEXP
    #rx"^\\\\node *\\(([0-9]+)\\) *(\\[.*\\])? *\\{\\\\rkt\\{([0-9]+)\\}\\{(.+)\\}\\};$"])
    (regexp-match: NODE_REGEXP "hai")
    (void))

  (let-regexp: ([EDGE_REGEXP
    #rx"^\\\\draw\\[.*\\]? *\\(([0-9]+)\\)[^(]*\\(([0-9]+)\\);$"])
    (regexp-match: EDGE_REGEXP "bye")
    (void))

  ;; -- check return type "inference"
  (check-equal?
    (regexp-match: #rx"hello" #"world")
    ;; Would be a type error if we annotated wrong
    #f)

  ;; -- starred group => 1 group
  (check-equal?
    (ann
      (regexp-match: "(poo )*" "poo poo platter")
      (U #f (List String (U #f String))))
   '("poo poo " "poo "))

  (check-equal?
    (ann
      (regexp-match: "([a-z ]+;)*" "lather; rinse; repeat;")
      (U #f (List String (U #f String))))
    '("lather; rinse; repeat;" " repeat;"))

  ;; -- ? = 1 group
  (let-regexp: ([date-re (pregexp: "([a-z]+) +([0-9]+,)? *([0-9]+)")])
    (check-equal?
      (ann
        (regexp-match: date-re "jan 1, 1970")
        (U #f (List String String (U #f String) String)))
      '("jan 1, 1970" "jan" "1," "1970"))

    (check-equal?
      (ann
        (regexp-match: date-re "jan 1970")
        (U #f (List String String (U #f String) String)))
      '("jan 1970" "jan" #f "1970"))
  )

  ;; -- (? = 0 groups
  (check-equal?
    (ann
      (regexp-match: "^(?:[a-z]*/)*([a-z]+)$" "/usr/local/bin/mzscheme")
      (U #f (List String String)))
    '("/usr/local/bin/mzscheme" "mzscheme"))

  (check-equal?
    (ann
      (regexp-match: #px"(?i:AloT)" "alot")
      (U #f (List String)))
    '("alot"))

  ; -- pipes = take min groups
  ;    2016-06-08: currently disabled
  (check-equal?
    (ann
      (regexp-match: "^(a*)|(b*)$" "aaa")
      (U #f (List String (U #f String) (U #f String))))
    '("aaa" "aaa" #f))

  (check-equal?
    (ann
      (regexp-match: "^(aa*)(c*)|(b*)$" "b")
      (U #f (List String (U #f String) (U #f String) (U #f String))))
    '("b" #f #f "b"))

  ;; -- nested gropus
  (check-equal?
    (ann
      (regexp-match: "((a)b)" "abc")
      (U #f (List String String String)))
    '("ab" "ab" "a"))
)
