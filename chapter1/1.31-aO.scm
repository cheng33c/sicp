#lang planet neil/sicp

(define (identity x) x) 
  
(define (next x) (+ x 1)) 
  
(define (factorial n) 
  (product identity 1 next n))
(define (product term a next b) 
  (if (> a b) 1 
      (* (term a) (product term (next a) next b))))
(define (pi-term n) 
  (if (even? n) 
      (/ (+ n 2) (+ n 1)) 
      (/ (+ n 1) (+ n 2))))

(* (product pi-term 1 next 6) 4)   ;;= 3.3436734693877552 
(* (product pi-term 1 next 1000) 4) ;;= 3.1570301764551676 