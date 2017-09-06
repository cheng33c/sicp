(load "3.13.scm")

(define (loop? my)
  (let ((ls '()))
    (define (iter current-my)
      (cond ((null? current-my) #f)
            ((eq? (car my) (car current-my)) #t)
            (else
             (begin
               (set! ls (cons ls (car current-my)))
               (iter (cdr current-my))))))
    (if (number? my)
        #f
        (iter (cdr my)))))

;; test example
(define z (make-cycle (list 'a 'b 'c)))
