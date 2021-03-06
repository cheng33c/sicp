#lang planet neil/sicp

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms
                      (sub-terms L1 (mul-terms L2 (list (make-term new-o new-c))))
                      L2)))
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly p1 p2) 
  (if (same-variable? (variable p1) (variable p2)) 
      (let ((results (div-terms (term-list p1) 
                                (term-list p2)))) 
        (list (make-poly (variable p1) (car results)) (cadr results))) 
      (error "Polys not in same var -- div-poly" 
             (list p1 p2))))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-list? a b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly a b)
  (if (same-variable? (variable a) (variable b))
      (make-poly (variable a)
                 (gcd-terms (term-list a)
                            (term-list b)))
      (error "ERROR gcd-poly --- not same variable" (list a b))))

(put 'greatest-command-divisor '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

(define (greatest-command-divisor p1 p2)
  (apply-generic 'greatest-command-divisor p1 p2))