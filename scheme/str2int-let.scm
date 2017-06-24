#lang planet neil/sicp

(define (str2int-let str)
  (let loop((ls (string->list str)) (sum 0))
    (if (null? ls)
        sum
        (loop (cdr ls) (+ (* sum 10) (- (char->integer (car ls)) 48))))))

(str2int-let "1234")