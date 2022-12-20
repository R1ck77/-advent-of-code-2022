;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 20 :example))
(defconst problem (advent/read-problem-lines 20 :problem))

(defun day20/read-problem (lines)
  (-map #'string-to-number lines))

(setq e (day20/read-problem example))
(setq p (day20/read-problem problem))

(defun day20/shift (list index steps)
  "Shift the values of the list at index for 'steps' posisionts.

Replicates the examples, which are a bit scant to be honest."
  (let ((new-index (mod (+ steps index) (1- (length list)))))
    (when (and (< steps 0) (zerop new-index))
      (setq new-index (1- (length list))))
    (-insert-at new-index (elt list index) (-remove-at index list))))

(defun day20/get-index (list index get-value-f)
  "General function to get the umpteenth value after 0 in a list.

The value of the list at position x is computed with get-value-f(x)"
  (let* ((corrected-list (-map get-value-f list))
         (zero-index (-elem-index 0 corrected-list)))
    (elt (-cycle corrected-list) (+ index zero-index))))

(defun day20/get-code (list get-value-f)
  "Compute the result of the problem for the list"
  (apply #'+ (--map (day20/get-index list it get-value-f) '(1000 2000 3000))))

(defstruct day20-data "Data required to move the list around"
           list
           values)

(defun day20/create-list-data (numbers)
  (make-day20-data :list (number-sequence 0 (1- (length numbers)))
                   :values numbers))

(defun day20/update-list (list-data new-list)
  (make-day20-data :list new-list
                   :values (day20-data-values list-data)))

(defun day20/mix-next (list-data index)
  (let ((old-list (day20-data-list list-data)))
    (day20/update-list list-data
                       (day20/shift old-list
                                    (-elem-index index old-list)
                                    (elt (day20-data-values list-data) index)))))

(defun day20/print-list-data (list-data)
  (print (--map (elt (day20-data-values list-data) it) (day20-data-list list-data))))

(defun day20/mix-list (list-data)
  (--dotimes (length (day20-data-values list-data))
    (setq list-data (day20/mix-next list-data it)))
  list-data)

(defun day20/part-1 (lines)
  (let ((mixed-list (day20/mix-list (day20/create-list-data (day20/read-problem lines))) ))
    (day20/get-code (day20-data-list mixed-list)
                    (lambda (x) (elt (day20-data-values mixed-list) x)))))

(defun day20/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day20)
