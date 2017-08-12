#lang planet neil/sicp

(define (entry tree) (car tree))

(define (left tree) (cadr tree))

(define (right tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))