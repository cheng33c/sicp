;; each-ripple-carry-adder-delay = full-adder-delay
;; = or-gate-delay + 2 * half-adder-delay
;; = or-gate-delay + 2*(or-gate-delay+2*and-gate-delay+inveter-delay)
;; = 3*or-gate-delay + 2*inveter-delay + 4*and-gate-delay

;; total-delay = n * each-ripple-carry-adder-delay
;; = 3*n*or-gate-delay + 2*n*inveter-delay + 4*n*and-gate-delay


;; implementation
(load "make-wire.scm")
(load "full-adder.scm")

(define (ripple-carry-adder table-a table-b table-s c)
  (define (iter current-a current-b current-s valc)
    (if (and (null? current-a) (null? current-b) (null? current-s))
        'ok
        (let ((ak (car current-a))
              (bk (car current-b))
              (sk (car current-s))
              (remain-a (cdr current-a))
              (remain-b (cdr current-b))
              (remain-c (cdr current-c))
              (ck (make-wire)))
          (set-signal! ck valc)
          (full-adder ak bk ck sk c)
          (iter remain-a remain-b remain-s (get-singal c)))))
  (iter table-a table-b table-s (get-singal c)))
