#lang at-exp racket/base

(provide (all-from-out "bib.rkt")
         (all-from-out scriblib/footnote)
         (all-from-out scriblib/figure)
         (all-from-out scribble/eval)
         (all-from-out scriblib/autobib)
         (except-out (all-from-out scribble/manual)
                     author)
         ~cite
         citet
         second
         etal
         exact
         generate-bibliography
         nrightarrow
         parag
         sf
         sc
         bot
         id
         todo
         proof
         )

(require "bib.rkt"
         racket/class
         racket/require
         scribble/core
         scribble/eval
         scribble/manual
         scriblib/autobib
         scriblib/figure
         scriblib/footnote
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties)

(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))

(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define small-number-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) ", ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i)
       (make-element
        (make-style "Thyperref" (list (command-extras (list (make-label i)))))
        (list (number->string i))))
     (define/public (render-author+dates author dates) dates)
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       (list (make-paragraph plain
                             (make-element colbibnumber-style
                                           (list
                                            (make-element (make-style "label" null)
                                                          (make-label i))
                                            "[" (number->string i) "]")))
             e))
     (super-new))))

(define author+date-style/link
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) bibentry-style)
     (define/public (disambiguate-date?) #t)
     (define/public (collapse-for-date?) #t)
     (define/public (get-cite-open) "(")
     (define/public (get-cite-close) ")")
     (define/public (get-group-sep) "; ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i)
       (make-element
        (make-style "Thyperref" (list (command-extras (list (make-label i)))))
        date-cite))
     (define/public (render-author+dates author dates)
        (list* author " " dates))
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       (list (make-compound-paragraph
              plain
              (list (make-paragraph plain (list (make-element (make-style "label" null)
                                                              (make-label i))))
                     e))))
     (super-new))))


(define-cite ~cite citet generate-bibliography
  ;; change this to small-number-style if you want the other way
  #:style small-number-style
  )

(define etal "et al.")

(define nrightarrow (elem #:style "mynra"))

(define (sf x) (elem #:style "sfstyle" x))

(define (sc x) (exact "\\textsc{" x "}"))

(define (parag . x) (apply elem #:style "paragraph" x))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define (mt-line) (parag))

(define (def #:term (term #false) . x)
  (make-paragraph plain
    (list
      (mt-line)
      (bold "Definition")
      (cons (if term (element #f (list " (" (defterm term) ") ")) " ") x)
      (mt-line))))

(define bot (exact "$\\bot$"))


;; Format an identifier
;; Usage: @id[x]
(define (id x)
  (make-element plain @format["~a" x]))

(define (todo . x*)
  (make-element 'bold (cons "TODO:" x*)))

(define (second)
  (make-element (make-style "relax" '(exact-chars))
                "$2^\\emph{nd}$"))

(define (defthing title #:thing thing #:tag tag . txt)
  ;; TODO use tag
  (nested #:style 'inset
    (list (bold thing " (" (emph title) ")" ":")
          (exact "~~")
          txt)))
          ;;(list txt))))

(define-syntax-rule (define-defthing* id* ...)
  (begin
    (begin
      (define (id* title #:tag [tag #f] . txt)
        (defthing title #:thing (string-titlecase (symbol->string (object-name id*))) #:tag tag txt))
      (provide id*)) ...))

(define-defthing* definition theorem lemma)

(define (proof . txt)
  (nested
    (list (emph "Proof:")
          (exact "~~")
          txt
          (exact "\\hfill\\qed"))))

