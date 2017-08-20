#lang planet neil/sicp

;; require

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol alpha tree)
  (define (encode-symbol-1 current-bit current-object)
    (if (leaf? current-object)
        (if (eq? (symbol-leaf current-object) alpha)
            (list current-bit)
            '())
        (let ((left-message (encode-symbol-1 0 (left-branch current-object)))
              (right-message (encode-symbol-1 1 (right-branch current-object))))
          (cond ((and (null? left-message) (null? right-message)) nil)
                ((null? left-message) (cons current-bit right-message))
                (else (null? right-message) (cons current-bit left-message))))))
  (let ((result (encode-symbol-1 '() tree)))
    (if (null? result)
        (error "Can't find " alpha)
        (cdr result))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (define (iter current-tree remain-leaves)
    (if (null? remain-leaves)
        current-tree
        (iter (make-code-tree (car remain-leaves)
                              current-tree)
              (cdr remain-leaves))))
  (iter (car leaves) (cdr leaves)))

;; main

(define my-set (list (list 'A 2) (list 'BOOM 1) (list 'NA 16)
                     (list 'SHA 3) (list 'GET 2) (list 'YIP 9)
                     (list 'JOB 2) (list 'WAH 1)))
(define my-huffman-tree (generate-huffman-tree my-set))
(define my-message '(
                     Get a job
                         Sha na na na na na na na na
                         Get a job
                         Sha na na na na na na na na
                         Wah yip yip yip yip yip yip yip yip yip
                         Sha boom))
(display (encode my-message my-huffman-tree))

;; 大概是在90位左右
;; 定长是 4 * 字母数