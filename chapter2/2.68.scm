#lang planet neil/sicp

;; 遍历操作，找到返回当前bit(0\1)和之前调用的bit组合
;; 没有找到返回'()，之前调用的因为返回了空所以它也返回空
;; 如果找到（不为空）就输出，否则就报错

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


;; problem provied
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; main
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

;; test
(define (sample-tree)
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(a d a b b c a))

(display (encode (list 'A 'D 'A 'B 'B 'C 'A) (sample-tree)))