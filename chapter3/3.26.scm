;;; make-table's first area cons with '*table* and sort method
;;; e.g. 'number 'dict 


(define (make-table sort-method)
  (let ((local-table (cons (list '*table* sort-method) '())))
    (define (compare a b flag)
      (cond ((eq? flag 'number)
	     (if (> a b) 'bigger 'smaller))))
    ;;; search key in records
    ;;; if find the key in local-table, than iteration
    ;;; else
    ;;; more than one key, compare first key and current key
    ;;; if bigger, than go to right branch
    ;;; else go to left branch
    (define (assoc key records)
      (cond ((null? records) false)
	    ((equal? key (caar records)) (car records))
	    (else
	     (let ((left-branch (cadr records))
		   (right-branch (cddr records))))
	     (if (eq? (compare (caar records) key) 'bigger)
		 (assoc key left-branch)
		 (assoc key right-branch)))))
    
    (define (lookup key-ls)
      (define (iter keys table)
	(cond ((null? keys) false)
	      ;;; left last one key, search it and return result
	      ((null? (cdr keys))
	       (let ((record (assoc (car keys) (cdr table))))
		 (if record
		     (cdr record)
		     (error "error lookup: can't find key " key))))
	     
	      (else
	       (let ((subtable (assoc (car keys) (cdr table))))
		 (if subtable
		     (iter (cdr keys) subtable)
		     false)))))
      (iter key-ls local-table))

    (define (insert! key-ls value)
      (define (iter keys table)
	(let ((current-key (car keys)))
	  (let ((current (assoc current-key table)))
	    (begin (if current
		       ;;; 1. find the key, add it
		       (let ((data-field (cdar current)))
			 (set! data-field (cons data-field value)))
		       (error "can't find the key" current-key))
		   ;;; if pair, continue
		   ;;; else end iteration
		   (if (pair? (cdr keys))
		       (iter (cdr keys) table)
		       table)))))
      (iter key-ls local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknow operation -- TABLE" m))))
    dispatch))


(define (lookup table . key-list) ((table 'lookup-proc) key-list)) 
(define (insert! table value . key-list) ((table 'insert-proc!) value key-list))


;; test
(define my (make-table 'number))
