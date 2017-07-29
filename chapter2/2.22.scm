#lang planet neil/sicp

(define (square x) (* x x))

;; 参考2.18-iter,其实这是2.18的平方版本
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; 展开表达式你会发现错误
(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; 这里有问题， e.g. (cons answer ...) =>
              ;; (cons (mcons 1) ...) => (mcons (mcons 1) 4) =>
              ;; (mcons (mcons (mcons 1) 4) 9) => ....
              (cons answer
                    (square (car things))))))
  (iter items nil))



(square-list (list 1 2 3 4 5))

(square-list1 (list 1 2 3 4 5))


;; right answer

(define (reverse lst)
    (iter lst '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))

(define (square-list2 items)
    (define (iter things answer)
        (if (null? things)
            (reverse answer) ; 修改
            (iter (cdr things)  
                  (cons (square (car things))
                        answer))))
    (iter items '()))

(square-list2 (list 1 2 3 4 5))



;; another right answer
(define (square-list4 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)  
                  (cons (square (car things))
                        answer))))
    (iter (reverse items) '()))  ; 修改

(square-list4 (list 1 2 3 4 5))