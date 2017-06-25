(define (square-apply ls)
  (sqrt (apply + (map (lambda (x) (* x x)) ls))))
