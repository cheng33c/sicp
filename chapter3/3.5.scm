(load "monte-carlo.scm")

;; problem provied
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; main
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo trials experiment)))
