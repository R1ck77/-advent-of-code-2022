(require 'dash)
(require 'advent-utils)

(defun count-depth-increment (acc value)
  (let ((start (car acc))
        (count (cadr acc)))
   (if (> value start)
       (setq count (1+ count)))
   (list value count)))

(defun day01/part-1 (input)
  (cadr
   (-reduce-from #'count-depth-increment (list (car input) 0) (cdr input))))

(defun day01/part-2 (input)
  (day01/part-1 (--map (apply #'+ it) (-partition-in-steps 3 1 input))))

(provide 'day01)
