(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-text 17 :example))
(defconst problem (advent/read-problem-text 17 :problem))

(defun day17/read-problem (line)
  (--map (if (string= "<" it) -1 1) (split-string (string-trim line) "" t)))

(defun day17/part-1 (lines)
  (error "Not yet implemented"))

(defun day17/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day17)
