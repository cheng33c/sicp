;;; insert key-value to table
;;; if find key in table, then add new value
;;; else add key-value in front of table


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)
