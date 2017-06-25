(define (sort-length ls)
  (sort ls (lambda (x y) (> (length x) (length y)))))
