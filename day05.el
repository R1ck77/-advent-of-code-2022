(require 'dash)
(require 's)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 5 :example nil t))
(defvar problem (advent/read-problem-lines 5 :problem nil t))

(defun day05/correct-counters (move)
  (seq-let (qt from to) move
      (list qt (1- from) (1- to))))

(defun day05/parse-moves (moves)
  (-map #'day05/correct-counters
        (--map (-map #'string-to-number
                     (rest (s-match "move \\(.+\\) from \\(.+\\) to \\(.+\\)" it)))
               moves)))

(defun day05/get-stacks (crates)
  (string-to-number (car (last (s-split " " (car (reverse crates)) t)))))

(defun day05/get-crate-at-pos (line pos)
  (assert (and (> pos 0) (< pos 10)) "Invalid pos")
  (char-to-string (elt line (+ 1 (* (1- pos) 4)) )))

(defun day05/parse-crates (crates)
  (let ((n (day05/get-stacks crates)))
    (assert (< n 10) "This program doesn't parse more than 9 stacks of crates correctly")
    (-drop-last 1 crates))
)

(defun day05/read-problem (lines)
  (seq-let (crates moves) (--split-when (equal "" it) lines)
    (list (day05/parse-crates crates)
          (day05/parse-moves moves))))

(defun day05/part-1 (lines)
  (error "Not yet implemented"))

(defun day05/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day05)
