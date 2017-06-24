#lang planet neil/sicp

(define (str2int str)
  (letrec ((iter (lambda (ls sum)
                   (if (null? ls)
                       sum
                       (iter (cdr ls)
                             (+ (* sum 10) (- (char->integer (car ls)) 48)))))))
    (iter (string->list str) 0)))

(str2int "123456")