#lang planet neil/sicp

(define (str2int str)
  (str2int-rec (string->list str) 0))

(define (str2int-rec str sum)
  (if (null? str)
      sum
      (let ((m (- (char->integer (car str)) 48)))
        (str2int-rec (cdr str) (+ (* sum 10) m) ))))

(str2int "1267")