#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ (if (null? x) 0 1) y))
              0 sequence))

(define (square x) (* x x))
(map square (list 1 2 3))

(append (list 1 2 3) (list 3 4 5))

(length (list 1 2 3 4 5))