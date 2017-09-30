(load "and-gate.scm")
(load "inverter.scm")
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (inverter (and-gate a1 a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
