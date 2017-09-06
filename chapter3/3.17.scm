(define (count-pairs x)
  (define (in-ls? ls x)
    (cond ((null? ls) #f)
          ((eq? (car ls) x) #t)
          (else (in-ls (cdr ls) x))))
  (let ((ls '()))
    (define (iter current-x counter)
      (cond ((null? current-x) counter)
            ((in-ls? ls (car x)) counter)
            (else
             (iter (cdr current-x) (+ counter 1)))))
    (iter x 0)))


;; test example
(define two (cons (cons 1 '()) '())) ;; 2
(define three (cons (cons 1 '()) (cons 2 '()))) ;; 3
(define seven (cons three three))

(count-pairs two) ;; 1
(count-pairs three) ;; 2
(count-pairs seven) ;; 3
