#lang planet neil/sicp

(define (str2int str)
  (do ((ls0 (string->list str) (cdr ls0))
       (sum 0 (+ (- (char->integer (car ls0)) 48)
               (* sum 10))))
    ((null? ls0) sum)))

(str2int "1234")