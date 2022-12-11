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
  (let ((operation (plist-get monkey :operation))
        (test-factor (plist-get monkey :test))
        (true-index (plist-get monkey :true))
        (false-index (plist-get monkey :false))
        (index (plist-get monkey :monkey)))
   (lambda (item)
     (funcall counter-bump-f index)
     (let ((result (/ (case (car operation)
                        (:square (* item item))
                        (:+ (+ item (cadr operation)))
                        (:* (* item (cadr operation)))
                        (t (error (format "Unexpected operation (%s)" operation))))
                      3)))
       (list result
             (if (day11/can-be-divided result test-factor)
                 true-index
               false-index))))))

(defun day11/prepare-no-stress-data (monkeys counter-update-f)
  (day11/prepare-data monkeys #'not
                      (lambda (_ item) item)
                      (lambda (_ monkey)
                        (day11/no-stress-monkey-function-generator nil monkey counter-update-f))))

(defun day11/move-item (monkey index all-items)
  (let* ((items (elt all-items index))
        (value-position (funcall monkey (car items))))
    (seq-let (value position) value-position      
      (-replace-at position
                   (append (elt all-items position) (list value))
                   (-replace-at index
                                (rest items)
                                all-items)))))

(defun day11/update-items (monkey index all-items)
  (let ((monkey-items (elt all-items index)))
    (if (not monkey-items)
      all-items
      (day11/update-items monkey
                          index
                          (day11/move-item monkey index all-items)))))

(defun day11/bump-counter (counter index)
  (aset counter index (1+ (aref counter index))))

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

(defun day11/extract-all-factors (raw-monkeys)
  (-uniq (--map (plist-get it :test) raw-monkeys)))

(defun day11/compute-reminders (factors item)
  (--map (cons it (mod item it)) factors))

(defun day11/update-reminder (operation factor reminder)
  (let ((operator (car operation))
        (op2 (cadr operation)))
   (case operator
     (:+ (mod (+ reminder op2) factor))
     (:* (mod (* reminder (mod op2 factor)) factor))
     (:square (mod (* reminder reminder) factor)))))

(defun day11/update-reminders (operation item)
  (-map (lambda (factor&reminder)
          (let ((factor (car factor&reminder))
                (reminder (cdr factor&reminder)))            
            (cons factor (day11/update-reminder operation factor reminder))))
        item))

(defun day11/stressful-monkey-function-generator (factors monkey counter-bump-f)
  (let ((operation (plist-get monkey :operation))
        (index (plist-get monkey :monkey))
        (test-divisor (plist-get monkey :test))
        (true-index (plist-get monkey :true))
        (false-index (plist-get monkey :false)))
   (lambda (item)
     (funcall counter-bump-f index)
     (let ((new-item (day11/update-reminders operation item)))
       (list new-item (if (zerop (cdr (assoc test-divisor new-item)))
                           true-index
                         false-index))))))

(defun day11/prepare-stressful-data (raw-monkeys counter-update-f)
  (day11/prepare-data raw-monkeys #'day11/extract-all-factors
                      #'day11/compute-reminders
                      (lambda (factors monkey)
                        (day11/stressful-monkey-function-generator factors
                                                                   monkey
                                                                   counter-update-f))))

(defun day11/part-2 (line-blocks)
  (let* ((raw-data (day11/read-data line-blocks))
         (counter (make-vector (length raw-data) 0))
         (bump-counter-f (lambda (index) (day11/bump-counter counter index))))
    (seq-let (items monkeys) (day11/prepare-stressful-data raw-data bump-counter-f)
      (day11/get-counter-for-rounds items monkeys 10000))
    (day11/compute-monkey-business counter)))

(provide 'day11)
