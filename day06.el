(require 'dash)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 6 :example))
(defvar problem (advent/read-problem-lines 6 :problem))

(defun day06/all-different? (group)
  (= (length group) (hash-table-count (advent/set-from group))))

(defun day06/get-packet-start-index (stream)
  (+ 4 (-find-index #'day06/all-different?
                    (-partition-in-steps 4 1 (split-string stream "" t)))))

(defun day06/part-1 (streams)
  (-map #'day06/get-packet-start-index streams))

(defun day06/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day06)
