#lang typed/racket/base

(provide Label Tree Node
  (struct-out label)
  (struct-out node)
  (struct-out suffix-tree)
  make-label
  set-label-i!
  set-label-j!
  make-suffix-tree
  make-node
  set-node-children!
  set-node-up-label!
  set-node-parent!
  set-node-suffix-link!)

(require "data.rkt")

(define-type Label label)
(define-type Tree suffix-tree)
(define-type Node node)

