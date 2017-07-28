#lang planet neil/sicp

(define (last-pair my)
  (if (null? (cdr my))
      ;; (list (car my))
      (car my)
      (last-pair (cdr my))))

(last-pair (list 23 72 149 34))