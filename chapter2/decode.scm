#lang planet neil/sicp

;; require
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))


;; main
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; test
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (weight-leaf x) (caddr x))

;; 遍历右树
(define my-bits (list 1))
;; 先遍历左树后右树
(define my-bits2 (list 0 1))
(define my-tree (make-code-tree (make-leaf 'A 4)
                                (make-code-tree
                                 (make-leaf 'B 2)
                                 (make-code-tree (make-leaf 'D 1)
                                                 (make-leaf 'C 1)))))

(display my-tree)
(newline)
(decode my-bits my-tree)
(newline)
(decode my-bits2 my-tree)