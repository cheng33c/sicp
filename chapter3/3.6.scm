(define rand
  (let ((range 1000000))
    (define (dispatch m)
      (cond ((eq? m 'generate) (random range))
            ((eq? m 'reset) (lambda (m) (set! range m) m))
            (else (error "ERROR DISPATCH -- UNKNOW MODE"))))
    dispatch))
