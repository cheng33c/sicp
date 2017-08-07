#lang planet neil/sicp

;; prime 
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (miller-rabin n)
  
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))

  (define (prime? n) 
    (define (fast-prime? n times)
      (cond ((= times 0) true)
            ((try-it n) (fast-prime? n (- times 1)))
            (else false)))
    (fast-prime? n 100))
  
  (define (try-it a)
    (if (= (expmod (+ 1 (random (- n 1))) (- n 1) n) 1)
        #t
        #f))

  (prime? n))

(define (filter proc sequence)
  (cond ((null? sequence) nil)
        ((proc (car sequence))
         (cons (car sequence)
               (filter proc (cdr sequence))))
        (else (filter proc (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      nil
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (miller-rabin (+ (car pair) (car (cdr pair)))))

(define (make-pair-sum pair)
  (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (make-pair-sum1 pair)
  (let ((first (car pair))
        (next (car (cdr pair))))
    (list first next (+ first next))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; unique-pairs --- let `prime-sum-pairs` easier
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; test unique-pairs 
;; (unique-pairs 5)

;; test `prime-sum-pairs` use `unique-pairs`
(prime-sum-pairs 6)