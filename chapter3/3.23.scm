(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-queue? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (rear-insert-deque! deque item))
        (else
         (set-front-ptr! deque (cons item (front-ptr deque)))
         deque)))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         deque)))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))



;;; O(n)'s solution
;;; O(1) need doubley list. see my another solution
(define (rear-delete-deque! deque)
  (define (iter current-deque new-deque)
    (if (null? (cddr current-deque))
        (begin
          (set-cdr! new-deque '())
          (set-rear-ptr! current-deque new-deque)
          deque)
        (iter current-deque (cdr new-deque))))
  (cond ((empty-deque? deque)
         (error "REAR-DELETE error -- deque empty" deque))
        ((null? (cdr (front-ptr deque)))
         (set-front-ptr! deque '())
         deque)
        (else
         (iter deque (front-ptr deque)))))

(define (print-deque deque)
  (car deque))

;; test example
(define d (make-deque))
(front-insert-deque! d 20)
(front-insert-deque! d 200003)
(rear-insert-deque! d 'a)
(rear-delete-deque! d)
