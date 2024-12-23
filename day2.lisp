(ql:quickload "iterate")
(ql:quickload "str")

(defpackage #:aoc2024
  (:use #:cl #:iterate))

(defun read-input ()
  (uiop:read-file-lines "./day2_input.txt"))

(defun parse-input (input)
  (mapcar
   (lambda (lst)
     (mapcar #'parse-integer (str:words lst)))
   input))

(defun list-differences (lst)
  (iterate
    (for i from 1 to (1- (length lst))) ; This gives number of elements! Not the final index...
    (collect (- (nth i lst) (nth (1- i) lst)))))

(defun positivep (num) ; Man...
  (if (> num 0)
      t
      nil))

(defun negativep (num)
  (if (< num 0)
      t
      nil))

(defun xor (x y) ; I'm surprised CL doesn't have this
  (not (eql x y)))

(defun safep (lst)
  (and
   (xor
    (every #'positivep lst)
    (every #'negativep lst))
   (every (lambda (num) (<= (abs num) 3)) lst)))

(defun answer (input)
  (count-if
   #'identity ; This is pretty neat
   (mapcar #'safep input)))

(answer (mapcar #'list-differences (parse-input (read-input))))
