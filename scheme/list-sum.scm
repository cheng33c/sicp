(define (ls-sum ls sum)
  (if (null? ls)
      sum
      (ls-sum (cdr ls) (+ sum (car ls)))))
