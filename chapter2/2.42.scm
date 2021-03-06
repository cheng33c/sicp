#lang planet neil/sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      nil
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter proc sequence)
  (cond ((null? sequence) nil)
        ((proc (car sequence))
         (cons (car sequence)
               (filter proc (cdr sequence))))
        (else (filter proc (cdr sequence)))))


(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (safe-check (car positions)
              (cdr positions)
              1))

(define (safe-check row-of-new-queen rest-of-queens i)
  (if (null? rest-of-queens)
      #t
      (let ((row-of-rest-queen (car rest-of-queens)))
        (if (or (= row-of-new-queen row-of-rest-queen)
                (= row-of-new-queen (+ row-of-rest-queen i))
                (= row-of-new-queen (- row-of-rest-queen i)))
            #f
            (safe-check row-of-new-queen
                        (cdr rest-of-queens)
                        (+ i 1))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(for-each (lambda (pos)
            (begin
              (display pos)
              (newline)))
          (queens 8))