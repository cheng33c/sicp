(define (my-length list count)
  (if (null? list)
      count
      (my-length (cdr list) (+ count 1))))
