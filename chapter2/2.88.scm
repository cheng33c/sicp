#lang racket

(define (sub-terms L1 L2)
  (cond ((empty-termlist? L1)
         (error "ERROR sub-terms --- UNKNOW L1"))
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((=zero? L1)
                  (adjoin-term
                   (make-term (order t1) (- (coeff t1)))
                   (sub-terms L1 (rest-term L2))))
                 ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (sub-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (sub-terms L1 (rest-terms L2))))
                 (else
                  (if (= (coeff t1) (coeff t2))
                      (sub-terms (rest-terms L1) (rest-terms L2))
                      (adjoin-term
                       (make-term (order t1)
                                  (sub (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2))))))))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; representation of terms and term lists
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)