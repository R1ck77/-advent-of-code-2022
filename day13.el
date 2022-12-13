(require 'dash)
(require 'advent-utils)
(require 's)

(defvar example (advent/read-blocks-of-lines 13 :example))
(defvar problem (advent/read-blocks-of-lines 13 :problem))

(defun day13/read-packet (line)
  (read (s-replace "," " " line)))

(defun day13/read-pair (line-block)
  (-map #'day13/read-packet line-block))

(defun day13/read-packets (line-blocks)
  (-map #'day13/read-pair line-blocks))

(defun day13/compare (left right)
  
  )

(defun day13/part-1 (lines)
  (error "Not yet implemented"))

(defun day13/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day13)
