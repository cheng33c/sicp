(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


;; test
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
