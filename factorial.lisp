;;;; factorial.lisp

;; Simple method for evaluating a number's factorial:
;(defun factorial (n)
;  (if (= n 0) 1
;    (* n (factorial (- n 1)))))

;; Faster version (generally), assuming tail recursion optimization:
(defun factorial (n &optional (acc 1))
  (if (= n 0) acc
    (factorial (- n 1) (* acc n))))

;; Iterative version using Common Lisp's loop macro:
;(defun factorial (n)
;  (loop for i from 1 to n
;    for fac = 1 then (* fac i)
;    finally (return fac)))
