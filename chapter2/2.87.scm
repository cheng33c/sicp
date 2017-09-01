#lang planet neil/sicp

(define (=zero? p)
  (cond ((number? p) (= p 0))
        ((pair? p) false)
        (else
         (error "ERROR =zero? -- UNKNOW TYPE"))))