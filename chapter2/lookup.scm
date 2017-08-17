#lang planet neil/sicp

(define (key my) (car my))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(lookup 'c (list (list 'a 20) (list 'b 30) (list 'c 60)))