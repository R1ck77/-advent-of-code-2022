(require 'dash)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 9 :example))
(defvar problem (advent/read-problem-lines 9 :problem))

(defconst day09/string-to-direction '(("R" . :right)
                                      ("L" . :left)
                                      ("U" . :up)
                                      ("D" . :down)))

(defun day09/read-line (line)
  (seq-let (string-dir count) (split-string line)
    (-repeat (string-to-number count)
             (cdr (assoc string-dir day09/string-to-direction)))))

(defun day09/read-problem (lines)
  (-mapcat #'day09/read-line lines))

(defun day09/part-1 (lines)
  (error "Not yet implemented"))

(defun day09/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day09)
