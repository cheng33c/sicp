(define (member-if proc ls)
  (cond
   ((null? ls) #f)
   ((proc (car ls)) ls)
   (else (member-if proc (cdr ls)))))

(member-if positive? '(0 -1 -2 3 5 -7))
