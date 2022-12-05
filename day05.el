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
  (assert (and (>= pos 0) (< pos 9)) "Invalid pos")
  (let ((letter (char-to-string (elt line (+ 1 (* pos 4)) ))))
    (unless (string= letter " ") letter)))

(defun day05/-read-crates-line (crates n line)
  (let ((new-crates ()))
    (--dotimes n
      (let ((stack (elt crates it)))
        (if-let ((crate (day05/get-crate-at-pos line it)))
            (setq stack (cons crate stack)))
        (setq new-crates (cons stack new-crates))))
    (nreverse new-crates)))

(defun day05/parse-crates (crates)
  (let* ((n (day05/get-stacks crates))
         (stacks (-repeat n '())))
    (assert (< n 10) "This program doesn't parse more than 9 stacks of crates correctly")
    (-map #'nreverse
           (--reduce-from  (day05/-read-crates-line acc n it)
                           (-repeat n '())
                           (-drop-last 1 crates)))))

(defun day05/read-problem (lines)
  (seq-let (crates moves) (--split-when (equal "" it) lines)
    (list (day05/parse-crates crates)
          (day05/parse-moves moves))))

(defun day05/update-crates (crates move reverse)
  (assert (/= (elt move 1) (elt move 2)))
  (seq-let (qt from to) move
    (let ((moved (-take qt (elt crates from))))
      (--map-indexed (cond
                      ((= it-index from) (-drop qt (elt crates from)))
                      ((= it-index to) (-concat (if reverse (reverse moved) moved) (elt crates to)))
                      (t it))
                     crates))))

(defun day05/move (state reverse)
  (seq-let (crates moves) state
    (list (day05/update-crates crates (car moves) reverse)
          (rest moves))))

(defun day05/predict-future-state (state &optional reverse)
  (while (cadr state)
    (setq state (day05/move state reverse)))
  (car state))

(defun day05/get-last-row (crates)
  (apply #'concat (-map #'car crates)))

(defun day05/part-1 (lines)
  (day05/get-last-row
   (day05/predict-future-state (day05/read-problem lines) t)))

(defun day05/part-2 (lines)
  (day05/get-last-row
   (day05/predict-future-state (day05/read-problem lines))))

(provide 'day05)
