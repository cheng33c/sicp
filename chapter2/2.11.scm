#lang planet neil/sicp

(define (make-interval upper-bound lower-bound)
  (cons lower-bound upper-bound))

(define (lower-bound x)
  (define (iter y min)
    (cond ((null? y) min)
          ((number? y) y)
          ((< (car y) min)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) min))))
  (iter (cdr x) (car x)))

(define (upper-bound x)
  (define (iter y max)
    (cond ((null? y) max)
          ((number? y) y)
          ((> (car y) max)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) max))))
  (iter (cdr x) (car x)))

;; child problem 1
;; upper: 上界 lower: 下界
;; 1： 同正或同0 0： 异号 -1： 同负
;; 固有11 -1-1 10 01 1-1 -11 -10 0-1 00 9种情况
;; 把它们全部考虑到，得到答案

(define (mul-interval x y)
  (define (ops-status upper lower)
    (cond ((and (>= upper 0) (>= lower 0)) 1)
          ((and (< upper 0) (< lower 0)) -1)
          (else 0)))
  (let ((x-lower (lower-bound x))
        (y-lower (lower-bound y))
        (x-upper (upper-bound x))
        (y-upper (upper-bound y)))
    (let ((opstatus-x (ops-status x-lower x-upper))
          (opstatus-y (ops-status y-lower y-upper)))
      (cond ((and (= opstatus-x 1) (= opstatus-y 1))
             (make-interval (* x-upper y-upper) (* x-lower y-lower)))
            ((and (= opstatus-x -1) (= opstatus-y -1))
             (make-interval (* x-lower y-lower) (* x-upper y-upper)))
            ((and (= opstatus-x 1) (= opstatus-y 0))
             (make-interval (* x-upper y-upper) (* x-upper y-lower)))
            ((and (= opstatus-x 0) (= opstatus-y 1))
             (make-interval (* x-upper y-upper) (* x-lower y-upper)))
            ((and (= opstatus-x 1) (= opstatus-y -1))
             (make-interval (* x-lower y-upper) (* x-upper y-lower)))
            ((and (= opstatus-x -1) (= opstatus-y 1))
             (make-interval (* x-upper y-lower) (* x-lower y-upper)))
            ((and (= opstatus-x -1) (= opstatus-y 0))
             (make-interval (* x-lower y-lower) (* x-lower y-upper)))
            ((and (= opstatus-x 0) (= opstatus-y -1))
             (make-interval (* x-lower y-lower) (* x-lower y-upper)))
            ((and (= opstatus-x 0) (= opstatus-y 0))
             (make-interval (* x-upper y-upper)
                            (* (max x-upper y-upper) (min x-lower y-lower))))))))




;;; test child problem 1
(mul-interval  '(+10 +10) '(+10 +10))
(mul-interval  '(+10 +10) '(+10 -10))
(mul-interval  '(-10 2) '(+10 +10))
(mul-interval  '(-10 -6) '(+10 +10))
(mul-interval  '(-3 7) '(-10 5))


;; 写上更简洁的写法
(define (mul-interval-clear x y)
  (define (ops-status upper lower)
    (cond ((and (>= upper 0) (>= lower 0)) 1)
          ((and (< upper 0) (< lower 0)) -1)
          (else 0)))
  (let ((x-lower (lower-bound x))
        (y-lower (lower-bound y))
        (x-upper (upper-bound x))
        (y-upper (upper-bound y)))
    (let ((opstatus-x (ops-status x-lower x-upper))
          (opstatus-y (ops-status y-lower y-upper)))
      (cond ((= opstatus-x 1)
             (cond ((= opstatus-y 1)
                    (make-interval (* x-upper y-upper) (* x-lower y-lower)))
                   ((= opstatus-y 0)
                    (make-interval (* x-upper y-upper) (* x-upper y-lower)))
                   ((= opstatus-y -1)
                    (make-interval (* x-lower y-upper) (* x-upper y-lower)))))
            ((= opstatus-x 0)
             (cond ((= opstatus-y 1)
                    (make-interval (* x-upper y-upper) (* x-lower y-upper)))
                   ((= opstatus-y 0)
                    (make-interval (* x-upper y-upper)
                                   (* (max x-upper y-upper) (min x-lower y-lower))))
                   ((= opstatus-y -1)
                    (make-interval (* x-lower y-lower) (* x-lower y-upper)))))
            ((= opstatus-x -1)
             (cond ((= opstatus-y 1)
                    (make-interval (* x-upper y-lower) (* x-lower y-upper)))
                   ((= opstatus-y 0)
                    (make-interval (* x-lower y-lower) (* x-lower y-upper)))
                   ((= opstatus-y -1)
                    (make-interval (* x-lower y-lower) (* x-upper y-upper)))))))))

(mul-interval-clear  '(+10 +10) '(+10 +10))
(mul-interval-clear  '(+10 +10) '(+10 -10))
(mul-interval-clear  '(-10 2) '(+10 +10))
(mul-interval-clear  '(-10 -6) '(+10 +10))
(mul-interval-clear  '(-3 7) '(-10 5))


;; 技巧性写法
(define (mul-interval-trick x y) 
  (define (endpoint-sign i)  
    (cond ((and (>= (upper-bound i) 0) 
                (>= (lower-bound i) 0)) 
           1) 
          ((and (< (upper-bound i) 0) 
                (< (lower-bound i) 0)) 
           -1) 
          (else 0))) 
  
  (let ((es-x (endpoint-sign x)) 
        (es-y (endpoint-sign y)) 
        (x-up (upper-bound x)) 
        (x-lo (lower-bound x)) 
        (y-up (upper-bound y)) 
        (y-lo (lower-bound y))) 
  
    (if (and (= es-x 0) (= es-y 0)) 
        ; Take care of the exceptional condition where we have to test 
        (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
                       (max (* x-lo y-lo) (* x-up y-up))) 
  
        ; Otherwise, select which value goes in which "slot". I'm not sure 
        ; whether there is an intuitive way to explain *why* these 
        ; selections work. 
        (let ((a1 (if (and (<= es-y 0) (<= (- es-y es-x) 0)) x-up x-lo)) 
              (a2 (if (and (<= es-x 0) (<= (- es-x es-y) 0)) y-up y-lo)) 
              (b1 (if (and (<= es-y 0) (<= (+ es-y es-x) 0)) x-lo x-up)) 
              (b2 (if (and (<= es-x 0) (<= (+ es-x es-y) 0)) y-lo y-up))) 
          (make-interval (* a1 a2) (* b1 b2)))))) 

(mul-interval-trick  '(+10 +10) '(+10 +10))
(mul-interval-trick  '(+10 +10) '(+10 -10))
(mul-interval-trick  '(-10 2) '(+10 +10))
(mul-interval-trick  '(-10 -6) '(+10 +10))
(mul-interval-trick  '(-3 7) '(-10 5))