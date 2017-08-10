#lang planet neil/sicp


;; require
(define (make-vect x y) (list x y))

(define (make-segment vec1 vec2)
  (list vec1 vec2))

(define (xcor-vect vec) (car vec))

(define (ycor-vect vec) (cadr vec))

(define (origin-frame vec) (car vec))

(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (caddr frame))

(define (add-vect vec1 vec2)
  (cons (+ (xcor-vect vec1) (xcor-vect vec2))
        (+ (ycor-vect vec1) (ycor-vect vec2))))


(define (scale-vect s vec)
  (cons (* (xcor-vect vec) s)
        (* (ycor-vect vec) s)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))




;; answer
(define (draw-outline frame)
  ((segments->painter 
     (list (make-segment (make-vect 0 0)
                         (make-vect 0 1))
           (make-segment (make-vect 0 1)
                         (make-vect 1 1))
           (make-segment (make-vect 1 1)
                         (make-vect 1 0))
           (make-segment (make-vect 1 0)
                         (make-vect 0 0))))
   frame))

(define (draw-x frame)
  ((segments->painter
     (list (make-segment (make-vect 0 0)
                         (make-vect 1 1))
           (make-segment (make-vect 1 0)
                         (make-vect 0 1))))
   frame))

(define (draw-diamond frame)
  ((segments->painter
     (list (make-segment (make-vect 0 0.5)
                         (make-vect 0.5 0))
           (make-segment (make-vect 0.5 0)
                         (make-vect 1 0.5))
           (make-segment (make-vect 1 0.5)
                         (make-vect 0.5 1))
           (make-segment (make-vect 0.5 1)
(make-vect 0 0.5))))))

(draw-outline (list 1 2 3 4))