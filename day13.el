(require 'dash)
(require 'advent-utils)
(require 's)

(defconst divider-packet-1 '((2)))
(defconst divider-packet-2 '((6)))

(defun day13/read-packet (line)
  (read
   (s-replace-all '(("," . " ")
                    ("[" . "(")
                    ("]" . ")"))
                  line)))

(defun day13/read-pair (line-block)
  (-map #'day13/read-packet line-block))

(defun day13/read-packets (line-blocks)
  (-map #'day13/read-pair line-blocks))

(defun day13/compare-lists (left right)
  (let ((result '(nil nil)))
    (while (not (or (car result) (cadr result)))
      (sit-for 0)
      (setq result (cond
                    ((not (or left right)) '(nil t))
                    ((not left) '(:ok nil))
                    ((not right) '(:error nil))
                    (t (let ((a (car left))
                             (b (car right)))
                         (setq left (rest left))
                         (setq right (rest right))
                         (list (day13/compare a b) nil))))))
    (car result)))

(defun day13/compare-left-list (left right)
  (if (numberp right)
      (day13/compare left (list right))
    (day13/compare-lists left right)))

(defun day13/compare-left-number (left right)
  (if (numberp right)
      (cond
       ((< left right) :ok)
       ((= left right) nil)
       ((> left right) :error))
    (day13/compare (list left) right)))

(defun day13/compare (left right)
  (if (listp left)
      (day13/compare-left-list left right)
    (day13/compare-left-number left right)))

(defun day13/part-1 (lines)
  (apply #'+ (-map #'car
                   (--filter (eq (cdr it) :ok)
                             (--filter (progn
                                         (advent/assert (cdr it) (format "Nil result found (block %d)!!!" (1- (car it))))
                                         t)
                                       (--map-indexed (cons (1+ it-index) it)
                                                      (--map (apply #'day13/compare it)
                                                             (day13/read-packets lines))))))))

(defun day13/sort-all-packets (packets)
  (--sort (eq (day13/compare it other) :ok)
          packets))

(defun day13/in-order? (packets)
  (equal (-uniq
          (--map (apply #'day13/compare it)
                 (-partition-in-steps 2 1 packets)))
         '(:ok)))

(defun day13/print-comparisons (packets &optional print-both)
  (--map (if print-both
             (cons (apply #'day13/compare it) it)
           (apply #'day13/compare it))
         (-partition-in-steps 2 1 packets)))

(defun day13/read-packets-list (line-blocks)
  (-flatten-n 1 (day13/read-packets line-blocks)))

(defun day13/insert-packets (packets)
  (day13/sort-all-packets (cons divider-packet-2 (cons divider-packet-1 packets))))

(defun day13/compute-inserted-packets-indices-product (packets)
  (apply #'* (-map #'car (-filter #'cadr (--map-indexed (list (1+ it-index)
                                                              (or (equal divider-packet-1 it)
                                                                  (equal divider-packet-2 it)))
                                                        packets)))))

(defun day13/part-2 (line-blocks)
  (day13/compute-inserted-packets-indices-product
   (day13/insert-packets
    (day13/read-packets-list line-blocks))))

(provide 'day13)
