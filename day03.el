(require 'dash)
(require 'advent-utils)

(defconst day03/all-lowercase (-map #'char-to-string (number-sequence 97 122)))
(defconst day03/all-uppercase (-map #'char-to-string (number-sequence 65 90)))

(defun day03/read-items (rucksack)
  (let ((elements (--filter (> (length it) 0)
                            (split-string rucksack ""))))
    (-split-at (/ (length elements) 2) elements)))

(defun day03/read-packing (packings)
  (-map #'day03/read-items packings))

(defun day03/find-duplicate (list-a list-b)
  (let ((ref (advent/set-from list-a)))
    (car (--filter (advent/get ref it) list-b))))

(defun day03/score-letter (letter)
  (let ((ascii-code (string-to-char letter)))
    (if (>= ascii-code 97)
        (+ (- ascii-code 97) 1)
      (+ (- ascii-code 65) 27))))

(defun day03/score-error (bag1-bag2)
  (day03/score-letter
   (day03/find-duplicate (car bag1-bag2)
                         (cadr bag1-bag2))))

(defun day03/part-1 (lines)
  (apply #'+ (-map #'day03/score-error (day03/read-packing lines))))

(defun day03/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day03)
