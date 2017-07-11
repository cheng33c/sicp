(define (pascal col row)
    (if (or (= col 1) (= col row))
        1
        (+ (pascal (- col 1) (- row 1)) (pascal col (- row 1)))))

(define (pascal1 col row)
    (cond ((> col row) 'Wrong)
          ((or (= col 1) (= col row)) 1)
          (else (+ (pascal (- col 1) (- row 1)) (pascal col (- row 1))))))