(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (new-good-enough? new-guess old-guess x)
    (< (abs (/ (- old-guess new-guess) new-guess)) 0.01))

(define (new-sqrt-iter new-guess old-guess x)
    (if (new-good-enough? new-guess old-guess  x)
        new-guess
        (sqrt-iter (improve guess x) guess
                    x)))

(define (new-sqrt x)
    (sqrt-iter 1.0 x))