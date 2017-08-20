#lang planet neil/sicp

;; require

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; problem proived

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; main
(define (successive-merge leaves)
  (define (iter current-tree remain-leaves)
    (if (null? remain-leaves)
        current-tree
        (iter (make-code-tree (car remain-leaves)
                              current-tree)
              (cdr remain-leaves))))
  (iter (car leaves) (cdr leaves)))


;; test

(define my-set (list (list 'a 80) (list 'b 30) (list 'c 40)))
(display (generate-huffman-tree my-set))