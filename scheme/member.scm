#lang planet neil/sicp

(define (member proc obj ls)
  (cond
    ((null? ls) #f)
    ((proc obj (car ls)) ls)
    (else (member proc obj (cdr ls)))))

(member string=? "hello" '("hi" "guys" "bye" "hello" "see you"))