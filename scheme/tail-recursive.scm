(define (fact-tail n)
 (fact-rec n n))

(define (fact-rec n p)
 (if (= n 1)
  p
  (let ((m (- n 1)))
   (fact-rec m (* p m)))))

(define (my-reverse ls)
 (my-reverse-tail ls ()))

(define (my-reverse-tail ls0 ls1)
 (if (null? ls0)
  ls1
  (my-reverse-tail (cdr ls0) (cons (car ls0) ls1))))

(define (sum-of-list ls)
 (sum-of-list-tail ls 0))

(define (sum-of-list-tail ls sum)
 (if (null? ls)
  sum
  (sum-of-list-tail (cdr ls) (+ sum (car ls)))))

(define (str-2-int ls)
 (str-2-int-tail ls 0))

(define (str-2-int-tail ls sum)
 (if (null? ls)
  sum
  (str-2-int-tail (cdr ls) 
   (+ (* sum 10) (- (char->integer (car ls)) 48)))))
