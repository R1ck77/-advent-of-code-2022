;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)

(setq example (advent/read-problem-text 11 :example))
(setq problem (advent/read-problem-text 11 :problem))

(setq e (eval (read example)))
(setq p (eval (read problem)))

(defun day11/extract-monkeys (monkey-items)
  (let ((operation (plist-get monkey-items :operation))
        (test (plist-get monkey-items :test)))
   (lambda (old)
     (let ((new (/ (funcall operation old) 3)))
       (list new (funcall test new))))))

(defun day11/extract-items (monkey-items)
  ;(apply #'vector (plist-get monkey-items :items))
  (plist-get monkey-items :items))

(defun day11/separate-data-from-functions (monkeys-items)
  (list (-map #'day11/extract-monkeys monkeys-items)
        (-map #'day11/extract-items monkeys-items)))

(defun day11/replace-element (a-list index new-value)
  (let ((copy (apply #'list a-list)))
    (setcar (nthcdr index copy) new-value)
    copy))

(defun day11/move-item (monkeys index all-items)
  (let* ((items (elt all-items index))
        (value-position (funcall (elt monkeys index) (car items))))
    (seq-let (value position) value-position      
      (advent/list-replace (advent/list-replace all-items index (rest items))
                           position
                           (append (elt all-items position) (list value))))))

(defun day11/update-items (monkeys index all-items)
  (let ((monkey-items (elt all-items index)))
    (if (not monkey-items)
      all-items
      (day11/update-items monkeys index (day11/move-item monkeys index all-items)))))



(defun day11/part-1 (text)

  (error "Not yet implemented"))

(defun day11/part-2 (text)
  (error "Not yet implemented"))

(provide 'day11)
