(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" queue))
            (else
             (begin
               (set! front-ptr (cdr front-ptr))
               front-ptr))))
    (define (empty-queue?) (null? front-ptr))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'print-queue) (print-queue))
            (else "ERROR make-queue --- unknow operation")))
    dispatch))


;; test example
(define q (make-queue))
((q 'insert-queue!) 100)
((q 'insert-queue!) 'a)
