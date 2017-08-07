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

(define (sum-s? pair s)
  (let ((first (car pair))
        (second (car (cdr pair)))
        (third (car (cdr (cdr pair)))))
    (= s (+ first second third))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; unique-pairs --- let `prime-sum-pairs` easier
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))


(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)                   ; cons 起 i 元素和二元组 j ,组成三元组
                    (cons i j))
                  (unique-pairs (- i 1))))      ; 生成不大于 i 的所有相异整数二元组
           (enumerate-interval 1 n)))             ; 生成 1 至 n 的所有整数，作为 i

(define (prime-sum-pairs n s)
  (map make-pair-sum
       (filter sum-s?
               (unique-triples n))))

(define (make-pair-sum pair)
  (let ((first (car pair))
        (second (car (cdr pair)))
        (third (car (cdr (cdr pair)))))
    (list first second third (+ first second third))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(prime-sum-pairs (unique-triples 13) 6)