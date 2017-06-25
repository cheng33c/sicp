(define (sort-sin ls)
  (sort ls (lambda (x y) (< (sin x) (sin y)))))
