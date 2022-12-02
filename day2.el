(require 'dash)
(require 'advent-utils)

(defconst day2/string-to-symbol '(("A" . :A)
                                  ("B" . :B)
                                  ("C" . :C)
                                  ("X" . :X)
                                  ("Y" . :Y)
                                  ("Z" . :Z)))

(defun day2/convert-token (token)
  (cdr (assoc token day2/string-to-symbol)))

(defun day2/read-moves (line)
  (let ((moves (-map #'day2/convert-token (split-string line))))
    (cons (car moves) (cadr moves))))

(defun day2/read-games (lines)
  (-map #'day2/read-moves lines))

(defun day2/play-game (input)
  )

(defun day2/part-1 (lines)
  (print (day2/read-games lines))
  (error "Not yet implemented"))

(defun day2/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day2)
