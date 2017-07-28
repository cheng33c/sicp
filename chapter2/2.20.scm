#lang planet neil/sicp

(define (same-parity . w)
  (define (reverse my ls n)
    (if (null? ls)
        my
        (if (odd? n)
            (reverse (cons my (car ls))
                     (cdr ls)
                     (+ n 1))
            (reverse my
                     (cdr ls)
                     (+ n 1)))))
  (reverse '() w 1))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7 8)

(same-parity)


;; 想出来的另一种
(define (same-parity1 . w)
  (define (reverse my ls)
    (cond ((null? ls) my)
          ((null? (cdr ls)) (cons my (car ls)))
        
          (else (reverse (cons my (car ls))
                         (cdr (cdr ls))))))
  (reverse '() w))

(same-parity1 1 2 3 4 5 6 7)

(same-parity1 2 3 4 5 6 7 8)

(same-parity1)