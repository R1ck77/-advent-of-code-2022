(require 'dash)
(require 'advent-utils)

(defun day01/part-1 (input)
  (car (-sort #'> (--map (apply #'+ it) input))))

(defun day01/part-2 (input)
  (apply #'+ (-take 3 (-sort #'> (--map (apply #'+ it) input)))))

(provide 'day01)
