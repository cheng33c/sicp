#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (col)
         (dot-product col v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (martix-*-martix m n)
  (let ((cols (transpose n)))
    (map (lambda (col-of-m)
           (map (lambda (col-of-cols)
                  (dot-product col-of-m
                               col-of-cols))
                cols))
         m)))

(define mylist (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(transpose mylist)

(martix-*-martix mylist (transpose mylist))

(define v (list 1 2 3 4))
(matrix-*-vector mylist v)