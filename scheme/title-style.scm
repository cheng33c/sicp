(define (identity x) x)

(define (title-style str)
  (let loop((ls (string->list str))
            (w #t)
            (acc '()))
    (if (null? ls)
        (list->string (reverse acc))
        (let ((c (car ls)))
          (loop (cdr ls)
                (char-whitespace? c)
                (cons ((if w char-upcase identity) c) acc))))))

(define (title-style str)
  (let ((n (string-length str)))
    (let loop ((w #t) (i 0))
      (if (= i n)
          str
          (let ((c (string-ref str i)))
            (if w (string-set! str i (char-upcase c)))
            (loop (char-whitespace? c) (+ 1 i)))))))

(title-style "the man name c")
