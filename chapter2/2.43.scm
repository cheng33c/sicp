#lang planet neil/sicp

(define (timed-time-test method n)
  (newline)
  (start-time-test n method (runtime)))

(define (start-time-test n method start-time)
  (if (for-each (lambda (pos)
                  (begin
                    (display pos)
                    (newline)))
                (method n))
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


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

(define (queens-old board-size)
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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;(for-each (lambda (pos)
  ;;          (begin
    ;;          (display pos)
      ;;        (newline)))
        ;;  (queens 8))

;; 注： 这里不准确。因为第一个函数有算启动时间，所以会特别长
(timed-time-test queens 8)  ;; 14133347
(newline)
(newline)
(timed-time-test queens-old 8) ;; 35308




;;queens 函数对于每个棋盘 (queen-cols k) ,使用 enumerate-interval 产生 board-size 个棋盘。
;;Louis的queens函数对于 (enumerate-interval 1 board-size) 的每个k,都产生(queen-cols (- k 1))个棋盘。
;;因此,Louis的queens函数的运行速度大约是原来queens函数的board-size倍，也即是 T * board-size 。