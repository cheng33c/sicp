(define (rtn-sum-of-two a b c)
 (cond 
  ((> a b) (cond ((> b c) (+ a b)))
            (else (+ a c)))
  ((> a c) (+ a b))
  (else (+ b c))
  ))
