#lang planet neil/sicp
(let ((i 1) (j 2))
  (+ i j))

(let ((i 1))
  (let ((j (+ i 2)))
    (* i j)))

(let ((i 2) (j 10))
  (/ j i))

(let* ((i 1) (j (+ i 2)))
  (* i j))
