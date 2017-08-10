#lang racket/gui

;; this code can draw beside
;; with the painter

;; require code
;; define painter
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))
(define (drawline begine end)
  (line (make-posn (car begine) (cadr begine)) 
        (make-posn (car end) (cadr end))))

(define (make-vect x y) (list x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cadr vect))
(define (add-vect vect1 vect2) (map + vect1 vect2))

(define (sub-vect vect1 vect2) (map - vect1 vect2))

(define (scale-vect n vect) (map (lambda (v) (* v n)) vect))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))
                  
(define (make-segment vector1 vector2) (list vector1 vector2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cadr seg))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (drawline
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))


(define wave-painter
  (let (
        (a (make-vect 0.4 0))
        (b (make-vect 0.6 0))
        (c (make-vect 0.25 0.2))
        (d (make-vect 0.75 0.2))
        (e (make-vect 0.45 0.4))
        (f (make-vect 0.65 0.4))
        (g (make-vect 0.75 0.4))
        (h (make-vect 0.35 0.4))
        (i (make-vect 0.25 0.35))
        (j (make-vect 0 0.2))
        (k (make-vect 1 0.7))
        (l (make-vect 0.3 0.7))
        (m (make-vect 1 0.8))
        (x (make-vect 0.35 0.45))
        (n (make-vect 0.4 0.5))
        (o (make-vect 0.65 0.6))
        (p (make-vect 0.35 1))
        (q (make-vect 0.5 1))
        (r (make-vect 0.55 0.75))
        (s (make-vect 0.6 1))
        (t (make-vect 0.7 1))
        (y (make-vect 0 0.3))
        (smile-p1 (make-vect 0.5 0.3))
        (smile-p2 (make-vect 0.55 0.35))
        (smile-p3 (make-vect 0.6 0.3)))
    (let ((segment-list
           (list (make-segment a c)
                 (make-segment b d)
                 (make-segment c e)
                 (make-segment d f)
                 (make-segment e h)
                 (make-segment f g)
                 (make-segment h i)
                 (make-segment i j)
                 (make-segment g k)
                 (make-segment y l)
                 (make-segment l x)
                 (make-segment x n)
                 (make-segment n p)
                 (make-segment q r)
                 (make-segment r s)
                 (make-segment t o)
                 (make-segment o m)
                 (make-segment smile-p1 smile-p2)
                 (make-segment smile-p2 smile-p3))))
      (segments->painter segment-list))))


;; test
;; define a frame where painter draw in and the size of frame
(define frame-a (make-frame (make-vect 0 200) (make-vect 200 0) (make-vect 0 200)))

(wave-painter frame-a)