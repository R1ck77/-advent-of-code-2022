(require 'dash)
(require 'advent-utils)

(defun day04/token-to-range (token)
  (-map #'string-to-number (split-string token "-")))

(defun day04/read-pairs (line)
  (-map #'day04/token-to-range (split-string line ",")))

(defun day04/read-problem (lines)
  (-map #'day04/read-pairs lines))

(defun day04/sort-ranges (ranges)
  (seq-let (a b) ranges
    (cond
     ((> (car a) (car b)) (list b a))
     ((< (car a) (car b)) (list a b))
     ((> (cadr a) (cadr b)) (list a b))
     (t (list b a)))))

(defun day04/is-subrange? (ranges)
  (seq-let (a b) (day04/sort-ranges ranges)
    (>= (cadr a) (cadr b))))

(defun day04/part-1 (lines)
  (length (-filter #'day04/is-subrange? (day04/read-problem lines))))

(defun day04/part-2 (lines)
  (error "Not yet implemented"))

(setq example (day04/read-problem (advent/read-problem-lines 4 :example)))
(setq problem (day04/read-problem (advent/read-problem-lines 4 :problem)))


(provide 'day04)
