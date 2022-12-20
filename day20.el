(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 20 :example))
(defconst problem (advent/read-problem-lines 20 :problem))

(defun day20/read-problem (lines)
  (-map #'string-to-number lines))

(setq e (day20/read-problem example))
(setq p (day20/read-problem problem))

(defun day20/shift (list index steps)
  (let ((new-index (mod (+ steps index) (1- (length list)))))
    (when (and (< steps 0) (zerop new-index))
      (setq new-index (1- (length list))))
    (-insert-at new-index (elt list index) (-remove-at index list))))

(defun day20/part-1 (lines)
  (error "Not yet implemented"))

(defun day20/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day20)
