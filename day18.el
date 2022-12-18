(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 18 :example))
(defconst problem (advent/read-problem-lines 18 :problem))

(defun day18/read-line (line)
  (-map #'string-to-number (split-string line ",")))

(defun day18/read-problem (lines)
  (-map #'day18/read-line lines))

(defun day18/part-1 (lines)
  (error "Not yet implemented"))

(defun day18/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day18)
