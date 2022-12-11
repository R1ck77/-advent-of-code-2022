;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)

(setq example (advent/read-blocks-of-lines 11 :example))
(setq problem (advent/read-blocks-of-lines 11 :problem))

(defun day11/read-last-int (string)
  (string-to-number
   (car
    (last
     (split-string string)))))

(defun day11/read-false-destination (false-str)
  (day11/read-last-int false-str))

(defun day11/read-true-destination (true-str)
  (day11/read-last-int true-str))

(defun day11/read-test (test-str)
  (day11/read-last-int test-str))

(defun day11/read-operation (operation-str)
  (let ((tokens (nthcdr 3 (split-string operation-str "[= ]+"))))
    (seq-let (op1 operator op2) tokens
      (advent/assert (or (not (string= op1 op2)) (string= operator "*")) "old + old not supported (or expected…)")
      (if (and (string= op1 op2))
          (list :square nil)
        (if (string= operator "+")
            (list :+ (string-to-number op2))
          (list :* (string-to-number op2)))))))

(defun day11/read-items (items-str)
  (-map #'string-to-number (rest (split-string items-str "[:,]"))))

(defun day11/read-monkey (monkey-str)
  (string-to-number (cadr (split-string monkey-str))))

(defun day11/read-block (blk)
  (seq-let (monkey-str items-str operation-str test-str true-str false-str) blk
    (list :monkey (day11/read-monkey monkey-str)
          :items (day11/read-items items-str)
          :operation (day11/read-operation operation-str)
          :test (day11/read-test test-str)
          :true (day11/read-true-destination true-str)
          :false (day11/read-false-destination false-str))))

(defun day11/read-data (blocks)
  (-map #'day11/read-block blocks))

(defun day11/create-monkey-functions (monkey preprocessed-data monkey-function-generator-f)
  (funcall monkey-function-generator-f preprocessed-data monkey))

(defun day11/create-item-list (monkey preprocessed-data item-to-object-f)
  (--map (funcall item-to-object-f preprocessed-data it)
         (plist-get monkey :items)))

(defun day11/prepare-data (monkeys preprocess-f item-to-object-f monkey-function-generator-f)
  (let ((preprocessed-data (funcall preprocess-f monkeys)))
    (list (--map (day11/create-item-list it preprocessed-data item-to-object-f)
                 monkeys)
          (--map (day11/create-monkey-functions it preprocessed-data monkey-function-generator-f)
                 monkeys))))

(defun day11/can-be-divided (a b)
  (zerop (mod a b)))

(defun day11/no-stress-monkey-function-generator (_ monkey counter-bump-f)
  ;; TODO/FIXME can be easily optimized (remove the cond/case/if and plist-get!)
  (lambda (item)
    (funcall counter-bump-f (plist-get monkey :monkey))
    (let ((operation (plist-get monkey :operation)))
     (let ((result (/ (case (car operation)
                        (:square (* item item))
                        (:+ (+ item (cadr operation)))
                        (:* (* item (cadr operation)))
                        (t (error (format "Unexpected operation (%s)" operation))))
                      3)))
    (if (day11/can-be-divided result (plist-get monkey :test))
        (list result (plist-get monkey :true))
      (list result (plist-get monkey :false)))       
       ))))

(defun day11/prepare-no-stress-data (monkeys counter-update-f)
  (day11/prepare-data monkeys #'not
                      (lambda (_ item) item)
                      (lambda (_ monkey)
                        (day11/no-stress-monkey-function-generator nil monkey counter-update-f))))

(defun day11/move-item (monkey index all-items)
  (let* ((items (elt all-items index))
        (value-position (funcall monkey (car items))))
    (seq-let (value position) value-position      
      (advent/list-replace (advent/list-replace all-items index (rest items))
                           position
                           (append (elt all-items position) (list value))))))

(defun day11/update-items (monkey index all-items)
  (let ((monkey-items (elt all-items index)))
    (if (not monkey-items)
      all-items
      (day11/update-items monkey
                          index
                          (day11/move-item monkey index all-items)))))

(defun day11/bump-counter (counter index)
  (aset counter index (1+ (aref counter index))))

;;; TODO/FIXME modifies counter but the name lies!
(defun day11/do-round (items monkeys)
  (list monkeys
        (-reduce-from (lambda (items monkey&index)
                        (day11/update-items (car monkey&index)
                                            (cdr monkey&index)
                                            items))
                      items
                      (--map-indexed (cons it it-index) monkeys))))

(defun day11/get-counter-for-rounds (items monkeys reps)
  (--dotimes reps
    (seq-let (_ new-items updated-counter) (day11/do-round items monkeys)
      (setq items new-items))))

(defun day11/compute-monkey-business (counter)
  (apply #'* (-take 2 (append (-sort #'> counter) nil))))


(defun day11/part-1 (line-blocks)
  (let* ((raw-data (day11/read-data line-blocks))
         (counter (make-vector (length raw-data) 0))
         (bump-counter-f (lambda (index) (day11/bump-counter counter index))))
    (seq-let (items monkeys) (day11/prepare-no-stress-data raw-data bump-counter-f)
      (day11/get-counter-for-rounds items monkeys 20))
    (day11/compute-monkey-business counter)))


(defun day11/part-2 (text)
    (let ((monkeys-items (eval (read text))))
    (seq-let (monkeys items) (day11/separate-data-from-functions monkeys-items #'identity)
        (day11/compute-monkey-business (day11/get-counter-for-rounds monkeys items 10000)))))

(provide 'day11)
