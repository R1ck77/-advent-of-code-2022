(require 'dash)
(require 'advent-utils)

(defconst day2/string-to-symbol '(("A" . :rock)
                                  ("B" . :paper)
                                  ("C" . :scissor)
                                  ("X" . :X)
                                  ("Y" . :Y)
                                  ("Z" . :Z)))

(defconst day2/win-rules '(((:rock . :rock) 3)
                           ((:rock . :paper) 6)
                           ((:paper . :rock) 0)
                           ((:scissor . :rock) 6)
                           ((:rock . :scissor) 0)
                           ((:paper . :paper) 3)
                           ((:paper . :scissor) 6)
                           ((:scissor . :paper) 0)
                           ((:scissor . :scissor) 3)))

(defconst day2/moves-values '((:rock . 1)
                              (:paper . 2)
                              (:scissor . 3)))

(defvar day2/moves-conversion '((:X . :rock)
                                (:Y . :paper)
                                (:Z . :scissor)))

(defun day2/convert-token (token)
  (cdr (assoc token day2/string-to-symbol)))

(defun day2/read-moves (line)
  (let ((moves (-map #'day2/convert-token (split-string line))))
    (cons (car moves) (cadr moves))))

(defun day2/read-games (lines)
  (-map #'day2/read-moves lines))

(defun day2/resolve-game (p1-p2)
  (cons (car p1-p2)
        (cdr (assoc (cdr p1-p2) day2/moves-conversion))))

(defun day2/play-game (game)
  (let ((resolved-moves (day2/resolve-game game)))
    (+ (cadr (assoc resolved-moves day2/win-rules))
       (cdr (assoc (cdr resolved-moves) day2/moves-values)))))

(defun day2/part-1 (lines)
  (apply #'+ (-map #'day2/play-game (day2/read-games lines))))

(defun day2/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day2)
