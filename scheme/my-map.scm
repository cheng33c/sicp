#lang planet neil/sicp

(define (my-map fun . lss)
  (letrec ((iter (lambda (fun lss)
                   (if (null? lss)
                       '()
                       (cons (fun (car lss))
                             (iter fun (cdr lss))))))
           (map-rec (lambda (fun lss)
                      (if (memq '() lss)
                          '()
                          (cons (apply fun (iter car lss))
                                (map-rec fun (iter cdr lss)))))))
    (map-rec fun lss)))

(my-map + '(1 2 3) '(10 20 30) '(100 200 300))