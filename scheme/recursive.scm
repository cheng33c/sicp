;; ch08
    
(define (list*2 ls)
 (if (null? ls)
  '()
  (cons (* 2 (car ls))
   (list*2 (cdr ls)))))

(define (my-length ls)
 (if (null? ls)
  0
  (+ (my-length (cdr ls)) 1)))

(define (sum-of-ls ls)
 (if (null? ls)
  0
  (+ (car ls) (sum-of-ls (cdr ls)))))

(define (pos-of-x-in-ls pos x ls)
    (cond
     ((null? ls) #f)
     ((eqv? x (car ls)) pos)
     (else (pos-of-x-in-ls (+ pos 1) x (cdr ls)))))

(define (del-element-in-list x ls)
 (if (null? ls)
  '()
  (let ((h (car ls)))
   ((if (eqv? x h)
     (lambda (y) y)
     (lambda (y) (cons h y)))
    (del-element-in-list x (cdr ls))))))
