#lang planet neil/sicp

(define (range x)
  (let loop((ls '()) (cursor 0))
    (if (eqv? x cursor)
        ls
        (loop (cons ls cursor) (+ cursor 1)))))

(range 8)