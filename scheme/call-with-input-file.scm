(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let loop((ls1 '()) (c (read-char p)))
        (if (eof-object? c)
            (begin
              (close-input-port p)
              (list->string (reverse ls1)))
            (loop (cons c ls1) (read-char p)))))))

(read-file "fly.scm")
