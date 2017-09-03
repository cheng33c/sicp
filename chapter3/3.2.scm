(define (make-monitored proc)
  (let ((times 0))
    (define (how-many-calls?) times)
    (define (apply-proc m)
      (begin (set! times (+ times 1))
             (proc m)))
    (define (reset-count)
      (begin (set! times 0)
             times))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (apply-proc m))))
    dispatch))


;; test example
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls)
(s 'reset-count)

