(require 'dash)
(require 'advent-utils)

(defconst day02/string-to-symbol '(("A" . :rock)
                                  ("B" . :paper)
                                  ("C" . :scissor)
                                  ("X" . :lose)
                                  ("Y" . :draw)
                                  ("Z" . :win)))

(defconst day02/win-rules '(((:rock . :rock) 3)
                           ((:rock . :paper) 6)
                           ((:paper . :rock) 0)
                           ((:scissor . :rock) 6)
                           ((:rock . :scissor) 0)
                           ((:paper . :paper) 3)
                           ((:paper . :scissor) 6)
                           ((:scissor . :paper) 0)
                           ((:scissor . :scissor) 3)))

(defconst day02/moves-values '((:rock . 1)
                              (:paper . 2)
                              (:scissor . 3)))

(defvar day02/guessed-moves-conversion '((:lose . :rock)
                                        (:draw . :paper)
                                        (:win . :scissor)))

(defconst day02/move-resolution '(((:rock . :win) :paper)
                                 ((:paper . :win) :scissor)
                                 ((:scissor . :win) :rock)
                                 ((:rock . :lose) :scissor)
                                 ((:paper . :lose) :rock)
                                 ((:scissor . :lose) :paper)
                                 ((:rock . :draw) :rock)
                                 ((:paper . :draw) :paper)
                                 ((:scissor . :draw) :scissor)))

(defun day02/convert-token (token)
  (cdr (assoc token day02/string-to-symbol)))

(defun day02/read-moves (line)
  (let ((moves (-map #'day02/convert-token (split-string line))))
    (cons (car moves) (cadr moves))))

(defun day02/read-games (lines)
  (-map #'day02/read-moves lines))

(defun day02/guessed-resolve-game (p1-p2)
  (cons (car p1-p2)
        (cdr (assoc (cdr p1-p2) day02/guessed-moves-conversion))))

(defun day02/play-game (game conversion-f)
  (let ((resolved-moves (funcall conversion-f game)))
    (+ (cadr (assoc resolved-moves day02/win-rules))
       (cdr (assoc (cdr resolved-moves) day02/moves-values)))))

(defun day02/part-1 (lines)
  (apply #'+ (--map (day02/play-game it #'day02/guessed-resolve-game)
                    (day02/read-games lines))))

(defun day02/correct-resolve-game (move-outcome)
  (let ((my-move (assoc move-outcome day02/move-resolution)))
    (cons (car move-outcome) (cadr my-move))))

(defun day02/part-2 (lines)
  (apply #'+ (--map (day02/play-game it #'day02/correct-resolve-game)
                    (day02/read-games lines))))

(provide 'day02)
