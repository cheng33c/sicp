(load "queue-ops.scm")

(define (print-queue q)
  (begin
    (display (car q))
    (print-queue (cdr q))))

