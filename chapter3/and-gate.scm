(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
        ((and (= a 1) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 0)
        ((and (= a 0) (= b 0)) 0)
        (else (error "ERROR logical-and -- Invalid signal"))))

