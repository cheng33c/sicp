;;; ben bitdiddle's solution
;;; it's wrong

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;;
(count-pairs '(1 2 3)) ;; return 3
(count-pairs '(1 'a)) ;; return 4
(count-pairs '('a 'b 2)) ;; return 7
(count-pairs '('(1 2) '(1 3) '(2 3))) ;; return 15
(count-pairs '('a 'b 'c)) ;; return 9



;;
(define two (cons (cons 1 '()) '())) ;; 2
(define three (cons (cons 1 '()) (cons 2 '()))) ;; 3
(define seven (cons three three))
