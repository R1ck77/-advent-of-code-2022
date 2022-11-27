(require 'picture)

(defun advent/goto (x y)
  "Goto to row y, column x

Pad the buffer with spaces if required"
  (goto-char (point-min))
  (picture-move-down y)
  (move-to-column x t))

(provide 'advent-extra-utils)
