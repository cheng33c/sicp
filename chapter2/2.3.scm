#lang planet neil/sicp

(define (perimeter-rectangle r)
  (let ((length (length-of-rectangle r))
        (width (width-of-rectangle r)))
    (* 2 (+ length width))))

(define (area-rectangle r)
  (* (length-of-rectangle r)
     (width-of-rectangle r)))

(define (make-rectangle length-1 length-2 width-1 width-2)
  (cons (cons length-1 length-2)
        (cons width-1 width-2)))

(define (length-1-rectangle r)
  (car (car r)))

(define (length-2-rectangle r)
  (cdr (car r)))

(define (length-1-rectangle r)
  (car (cdr r)))

(define (width-2-rectangle r)
  (cdr (cdr r)))