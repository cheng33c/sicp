#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr x)))))

(element-of-set? 5 '(5 6 7 8))