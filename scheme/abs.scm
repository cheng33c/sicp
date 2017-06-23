(define (abs x)
 (if (< x 0)
    (- x)
    x))

(define (daoshu x)
 (if (= x 0)
    #f
    (/ 1 x)))

(define (2ascii x)
 (if (or (< x 33) (> x 126) )
    #f
    (integer->char x)))
