#lang planet neil/sicp

(define (square x) (* x x))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment x1 y1 x2 y2)
  (cons (make-point x1 y1)
        (make-point x2 y2)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;;; 表示方式1： 用点来表示矩形
(define (distance-point p1 p2)
  (sqrt (+
         (square (- (x-point p1)
                    (x-point p2)))
         (square (- (y-point p1)
                    (y-point p2))))))

(define (area-rectangle p1 p2 p3)
  (* (distance-point p1 p2)
     (distance-point p1 p3)))

(define (perimeter-rectangle p1 p2 p3)
  (* (+ (distance-point p1 p2)
        (distance-point p1 p3)) 2))

(area-rectangle (make-point 1 1) (make-point 1 3) (make-point 3 1))
(perimeter-rectangle (make-point 1 1) (make-point 1 3) (make-point 3 1))

;; 表示方式2： 构造矩形用两个线段
(define (make-rectangle length-1 width-1 length-2 width-2)
  (cons (cons length-1 length-2)
        (cons width-1 width-2)))

(define (area-rectangle-seg seg-length seg-width)
  (* (distance-point (start-segment seg-width)
                     (end-segment seg-width))
     (distance-point (start-segment seg-length)
                     (end-segment seg-length))))

(define (perimeter-rectangle-seg seg-length seg-width)
  (* (+ (distance-point (start-segment seg-width)
                     (end-segment seg-width))
        (distance-point (start-segment seg-length)
                     (end-segment seg-length)))
     2))

(make-rectangle (make-segment 1 2 1 5)
                (make-segment 1 2 8 2)
                (make-segment 8 2 8 5)
                (make-segment 8 5 1 5))

(area-rectangle-seg (make-segment 1 2 1 5) (make-segment 1 2 8 2))