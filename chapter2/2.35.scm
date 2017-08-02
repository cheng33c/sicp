#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1)) t)))

(define mytree (list 1 2 (list 3 4 5) (list 6) (list 7) (list 8 2 5 3 9) 10 11 2))

(count-leaves mytree)