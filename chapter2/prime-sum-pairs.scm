#lang planet neil/sicp

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

(define (prime? n)
  (if (= (remainder n 2) 1)
      #t
      #f))

(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))

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
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))



;; lambda (i) test
((lambda (i)
   (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1)))) 4)

(prime-sum-pairs 6)