(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-text 17 :example))
(defconst problem (advent/read-problem-text 17 :problem))

(defconst day17/well-width 7)

(defconst day17/tiles '([[1 1 1 1]]
                  [[0 1 0]
                   [1 1 1]
                   [0 1 0]]
                  [[0 0 1]
                   [0 0 1]
                   [1 1 1]]
                  [[1]
                   [1]
                   [1]
                   [1]]
                  [[1 1]
                   [1 1]]))

(defun day17/read-problem (line)
  (--map (if (string= "<" it) -1 1) (split-string (string-trim line) "" t)))

(defun day17/generate-all-tiles (&optional count)
  (let ((repetitions (1+ (/ (or count 2022) (length day17/tiles)))))
    (-take count (-flatten (-repeat repetitions day17/tiles)))))


(defun day17/part-1 (lines)
  (error "Not yet implemented"))

(defun day17/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day17)
