#lang planet neil/sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana apple))

(memq 'apple '(x (apple sauce) y apple pear))