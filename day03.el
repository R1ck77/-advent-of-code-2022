(require 'dash)
(require 'advent-utils)

(defun day03/string-to-letters (string)
  (split-string string "" t))

(defun day03/read-items (rucksack)
  (let ((elements (day03/string-to-letters rucksack)))
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

(defun day03/read-groups (packings)
  (-partition 3 (-map #'day03/string-to-letters packings)))

(defun day03/read-common (a-b-c)
  (let* ((ref (advent/set-from (car a-b-c)))
         (ref2 (advent/set-from (--filter (advent/get ref it) (elt a-b-c 1)))))
    (car (--filter (advent/get ref2 it) (elt a-b-c 2)))))

(defun day03/part-2 (lines)
  (apply #'+
         (-map #'day03/score-letter
               (-map #'day03/read-common
                     (day03/read-groups lines)))))

(provide 'day03)
