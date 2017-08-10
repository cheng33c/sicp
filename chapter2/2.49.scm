#lang racket/gui

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "2.49 Solution" 800 800))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))
(define (drawline begine end)
  (line
   (make-posn (car begine) (cadr begine)) 
   (make-posn (car end) (cadr end))))

;; require
(define (make-vect x y) (list x y))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-segment vec1 vec2)
  (list vec1 vec2))

(define (xcor-vect vec) (car vec))

(define (ycor-vect vec) (cadr vec))

(define (origin-frame vec) (car vec))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (caddr frame))

(define (add-vect vec1 vec2)
  (map + vec1 vec2))

(define (sub-vect vec1 vec2)
  (map - vec1 vec2))

(define (scale-vect s vec)
  (map (lambda (v) (* s v)) vec))

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
       (drawline
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define outline-painter
  (let ((a (make-vect 0 0))
        (b (make-vect 0 1))
        (c (make-vect 1 0))
        (d (make-vect 1 1)))
    (let ((segment-list
           (list (make-segment a b)
                 (make-segment a c)
                 (make-segment b d)
                 (make-segment c d))))
      (segments->painter segment-list))))


(define fork-painter
  (let ((a (make-vect 0 0))
        (b (make-vect 0 1))
        (c (make-vect 1 0))
        (d (make-vect 1 1)))
    (let ((segment-list
           (list (make-segment a d)
                 (make-segment b c))))
      (segments->painter segment-list))))

(define diamond-painter
  (let ((a (make-vect 0 0.5))
        (b (make-vect 0.5 0))
        (c (make-vect 1 0.5))
        (d (make-vect 0.5 1)))
    (let ((segment-list
           (list (make-segment a b)
                 (make-segment a d)
                 (make-segment b c)
                 (make-segment c d))))
      (segments->painter segment-list))))

(define wave-painter
  (let ((a (make-vect 0.4 0))
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
        (x (make-vect 0.35 0.45)) ;; 这个点是忘记了，临时添加的
        (n (make-vect 0.4 0.5))
        (o (make-vect 0.65 0.6))
        (p (make-vect 0.35 1))
        (q (make-vect 0.5 1))
        (r (make-vect 0.55 0.75))
        (s (make-vect 0.6 1))
        (t (make-vect 0.7 1))
        (y (make-vect 0 0.3)))
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
                 (make-segment o m))))
      (segments->painter segment-list))))


;; define a frame where painter draw in and the size of frame
(define frame-a (make-frame (make-vect 0 200) (make-vect 200 0) (make-vect 0 200)))
(define frame-b (make-frame (make-vect 0 0) (make-vect 200 0) (make-vect 0 200)))
(define frame-c (make-frame (make-vect 0 400) (make-vect 200 0) (make-vect 0 200)))
(define frame-d (make-frame (make-vect 200 400) (make-vect 200 0) (make-vect 0 200)))

;; test painter
(outline-painter frame-a)
(fork-painter frame-b)
(diamond-painter frame-c)
(wave-painter frame-d)