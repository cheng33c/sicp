(define (my-copy-file from to)
  (let ((pfr (open-input-file from))
        (pto (open-output-file to)))
    (let loop((c (read-char pfr)))
      (if (eof-object? c)
          (begin
            (close-input-port pfr)
            (close-output-port pto))
          (begin
            (write-char c pto)
            (loop (read-char pfr)))))))
