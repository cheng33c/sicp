#lang planet neil/sicp

;; main part
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((current (car set))
            (remain (cdr set)))
        (cond ((= current x) set)
              ((< current x)
               (cons current
                     (adjoin-set x remain)))
              (else (cons x set))))))

(adjoin-set 50 '(1 2 3))
(adjoin-set 33 '(1 2 4 33 59))