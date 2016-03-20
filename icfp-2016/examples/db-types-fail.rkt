#lang typed/racket/base

;(define-new-subtype Database (database (Listof Table)))
;;; Issue 1: need (Listof T *)
;;(define-new-subtype Table (table (Pairof Symbol (Listof T))))
;(define-new-subtype Table (table (Pairof Symbol (Listof Any))))
;
;(define-type Me (Database (Table 'word (List Natural String))))
