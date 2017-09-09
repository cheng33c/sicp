;;; doubley link solution with O(1)

(define (make-two-way-deque) (cons (make-deque) (make-deque)))
(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-ptr (forward deque))))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))

(define (forward-deque two-way-deque)
  (if (pair? (car deque))
      (caar deque)
      (error "ERROR forward -- deque not have a forward list" reque)))
(define (reverse-deque two-way-deque)
  (if (pair? (cdr deque))
      (cadr deque)
      (error "ERROR reverse -- deque not have a reverse list" deque)))

;;; when two-way-deque is empty, insert new-pair to front-list and reverse-list
;;; to both forward-deque and reverse-deque
;;; when two-way-deque is not empty, insert new-pair to front in forward-deque
;;; insert new-pair to rear in reverse-deque
(define (front-insert-deque! two-way-deque)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! (forward-deque two-way-deque) new-pair)
	   (set-rear-ptr! (forward-deque two-way-deque) new-pair)
           (set-front-ptr! (reverse-deque two-way-deque) new-pair)
	   (set-rear-ptr! (reverse-deque two-way-deque) new-pair))
          (else
           (set-front-ptr! (forward-deque two-way-deque) new-pair)
           (set-rear-ptr! (reverse-deque two-way-deque) new-pair)))))

(define (rear-insert-deque! two-way-deque)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-rear-ptr! (forward-deque two-way-deque) new-pair)
	   (set-front-ptr! (reverse-deque two-way-deque) new-pair))
	  (else
	   (set-rear-ptr! (forward-ptr two-way-deque) new-pair)
	   (set-forward-ptr! (rear-ptr two-way-deque) new-pair)))))
