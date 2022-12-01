(require 'dash)
(require 'advent-utils)

(defun day01/compute-sorted-elves-calories (calories-list)
  (-sort #'> (--map (apply #'+ it) calories-list)))

(defun day01/part-1 (line-blocks)
  (car
   (day01/compute-sorted-elves-calories
    (advent/block-of-lines-to-numbers line-blocks))))

(defun day01/sum-first-three-values (numbers)
  (apply #'+ (-take 3 numbers)))

(defun day01/part-2 (line-blocks)
  (day01/sum-first-three-values
   (day01/compute-sorted-elves-calories
    (advent/block-of-lines-to-numbers line-blocks))))

(provide 'day01)
