#lang planet neil/sicp

;; required
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; problem provied

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))))

(define (deriv1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; problem a)
;; sicp中的定义：检查一个数据项的类型，并据此去调用某个适当的过程称为`基于类型的数据分派`
;; 从定义中可以知道我们必须先检查类型再将不同类型的数据进行分派，而不是先将数据进行分派在检查类型
;; 这是完全错误的设计。这样的设计会导致轻微的变动就要更改大量的代码的这种问题。

;; 这一段代码用于求导。 当exp为数字是导数为0。 当exp和var相同时（即要求导的式子和求导变量相同时）返回1
;; 否则分派给 'deriv 函数。 `operator`参数传递运算符 `operands`参数传递式子