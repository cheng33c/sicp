#lang planet neil/sicp

;; require
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (key my) (car my))

;; main
(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      #f
      (let ((entry-key (key (entry tree-of-records))))
        (cond ((equal? given-key entry-key) 
               (entry tree-of-records))
              ((> given-key entry-key)
               (lookup given-key (right-branch tree-of-records)))
              ((< given-key entry-key)
               (lookup given-key (left-branch tree-of-records)))))))


;; test

(define (make-tree entry left right)
  (list entry left right))

(define mytree (make-tree (list 30 'a)
                          (make-tree (list 21 'b) nil nil)
                          (make-tree (list 33 'c) nil nil)))


(lookup 30 mytree)