#lang planet neil/sicp

(define (fringe tree)
  (define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (if (pair? (car remained-items))
                  (iter (car remained-items) result)
                  (append result remained-items)))))
  (iter tree '()))

(define (fringe1 tree)
  (define (iter remained-items result)
    (cond ((null? remained-items) '())
          ((pair? remained-items)
           (iter (car remained-items) result))
          (else (append result remained-items))))
  (iter tree '()))

(define (fringe2 tree)
  (cond ((null? tree)                         ; 空树
         '())
        ((not (pair? tree))                   ; 叶子
         (list tree))
        (else
         (append (fringe2 (car tree))         ; 累积左子树所有元素
                 (fringe2 (cadr tree))))))    ; 累积右子树所有元素

(define x (list (list 1 2) (list 3 4)))
(fringe2 x)
(fringe2 (list x x))