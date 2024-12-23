(ql:quickload "iterate")

(defpackage #:aoc2024
  (:use #:cl #:iterate))

(defun read-input ()
  (iterate
    (for num in-file "./day1_input.txt")
    (for i first 1 then (1+ i))
    (if (oddp i)
	(collect num into list1)
	(collect num into list2))
    (finally (return (list list1 list2)))))

(defun sort-input (input)
  (list
   (sort (first input) #'<)
   (sort (second input) #'<)))

; Could just be one nested loop
(defun answer-part1 (input)
  (iterate
    (for num1 in (first input))
    (for num2 in (second input))
    (sum (abs (- num2 num1)))))

; Can memoize
(defun answer-part2 (input)
  (iterate
    (for num in (first input))
    (sum (* num (funcall #'count num (second input))))))

(answer-part1 (sort-input (read-input)))
(answer-part2 (read-input))
