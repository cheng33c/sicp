(define (make-accumulator balance)
  (define (accumulator amount)
    (set! balance (+ balance amount))
    balance)
  (define dispatch accumulator)
  dispatch)

(define A (make-accumulator 5))
(A 10)
