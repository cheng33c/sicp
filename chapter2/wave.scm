#lang planet neil/sicp

(define (wave2)
  (beside wave (flip-vert wave)))

(define (wave4)
  (blow wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2))) 

(define wave4
  (flipped-pairs wave))

(wave4)