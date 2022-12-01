(require 'dash)
(require 'advent-utils)

(defun day01/part-1 (input)
  (car (-sort #'> (--map (apply #'+ it) input))))

(defun day01/part-2 (input)
  12)

(provide 'day01)
