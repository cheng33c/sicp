(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; `z`出现了abc死循环输出
;; 原因在于c的尾指针又指向了最前面也就是a
;; 所以输出时不停的循环
