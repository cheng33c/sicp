;;; lookup key in two-dimension table
;;; because lookup in two-dimension table
;;; so user should provied two key
;;; if find key-1 and key-2 then return key's value
;;; else return false

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))
