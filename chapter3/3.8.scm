(define f
  (let ((called #f))
    (lambda (n)
      (if called
          0
          (begin (set! called #t)
                 n)))))
