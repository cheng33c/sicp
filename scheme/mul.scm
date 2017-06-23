(define (mul1 a b c)
 (and (> a 0) (> b 0) (> c 0)
  (* a b c)
    #f))

(define (mul2 a b c)
 (if (or (< a 0) (< b 0) (< c 0))
  (* a b c)
    #f))
