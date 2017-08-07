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

(define (permutations s)
  (if (null? s)
      (list nil)
      ;; 返回当前所有组合的方法
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; lambda x test
((lambda (x)
   (map (lambda (p) (cons x p))
        (permutations (remove x (list 1 2 3 4)))))
 3)

(permutations (list 1 2 3))