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

(defun day11/move-item (monkey index all-items &optional inspections-f)
  (funcall (or inspections-f #'identity) index)
  (let* ((items (elt all-items index))
        (value-position (funcall monkey (car items))))
    (seq-let (value position) value-position      
      (advent/list-replace (advent/list-replace all-items index (rest items))
                           position
                           (append (elt all-items position) (list value))))))

(defun day11/update-items (monkey index all-items &optional inspection-f)
  (let ((monkey-items (elt all-items index)))
    (if (not monkey-items)
      all-items
      (day11/update-items monkey
                          index
                          (day11/move-item monkey index all-items inspection-f)
                          inspection-f))))

(defun day11/bump-counter (counter index)
  (aset counter index (1+ (aref counter index))))

;;; TODO/FIXME modifies counter but the name lies!
(defun day11/do-round (monkeys items &optional counter)
  (let ((counter (or counter (make-vector (length monkeys) 0))))
    (list monkeys
          (-reduce-from (lambda (items monkey&index)
                    (let ((index (cdr monkey&index)))
                      (day11/update-items (car monkey&index)
                                          index
                                          items
                                          (lambda (index) (day11/bump-counter counter index)))))
                  items
                  (--map-indexed (cons it it-index) monkeys))
          counter)))

(defun day11/get-counter-for-rounds (monkeys items index)
  (let ((counter))
   (--dotimes index
     (seq-let (_ new-items updated-counter) (day11/do-round monkeys items counter)
       (setq items new-items)
       (setq counter updated-counter)) ;; TODO/FIXME geez
     )
   counter))

(defun day11/compute-monkey-business (counter)
  (apply #'* (-take 2 (append (-sort #'> counter) nil))))


(defun day11/part-1 (text)
  (let ((monkeys-items (eval (read text))))
    (seq-let (monkeys items) (day11/separate-data-from-functions monkeys-items)
        (day11/compute-monkey-business (day11/get-counter-for-rounds monkeys items 20)))))

(defun day11/part-2 (text)
  (error "Not yet implemented"))

(provide 'day11)
