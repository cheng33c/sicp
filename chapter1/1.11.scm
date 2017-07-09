;;; 递归
(define (f-rec n)
    (if (< n 3) 
        n
        (+ (f-rec (- n 1)) (* (f-rec (- n 2)) 2) (* (f-rec (- n 3)) 3))))

;;; 迭代
(define (f-iter n)
    (define (my-f n sum)
        (if (< n 3)
            sum
            (my-f (- n 1) (+ sum (- n 1) (* (- n 2) 2) (* (- n 3) 3)))))
    (my-f n 0))

