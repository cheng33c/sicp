(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
            (else else-clause)))


(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                        x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))


;;; 解释： new-if采用了应用序求值， 首先计算了分支sqrt-iter的值， 但是sqrt-iter分支很深，所以产生了溢出。
;;; 比如我写的如下程序，同样产生了溢出。因为程序优先运算了两个结果的值， 但第二个结果是个递归，如果进入就出不来了。
;;; 所以报出了 “;Aborting!: maximum recursion depth exceeded” 错误

(define (my-test x)
    (new-if (< 0 x)
            x
            (my-test x)))

;;; 但是 if 没有如下情况。显然 if 是先计算表达式的值，然后选择跳转分支（正则序求值）。
;;; 当 x小于 0时，进入了无限的递归没有相应。 当 x大于0时，返回了 x的值

(define (my-test2 x)
    (if (< 0 x)
        x
        (my-test2 x)))


;;; 从本例出可以看出正则序和应用序的优缺点。