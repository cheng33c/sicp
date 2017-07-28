#lang planet neil/sicp

(define (length-iter items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter items 0))

(length-iter (list 1 2 3 4))