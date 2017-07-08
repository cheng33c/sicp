(define (print-lines . lines)
  (let loop((ls0 lines))
    (if (pair? ls0)
        (begin
          (display (car ls0))
          (newline)
          (loop (cdr ls0))))))
