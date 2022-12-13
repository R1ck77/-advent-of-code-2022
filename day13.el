(require 'dash)
(require 'advent-utils)
(require 's)

;;; 5081 too low

(defvar example (advent/read-blocks-of-lines 13 :example))
(defvar problem (advent/read-blocks-of-lines 13 :problem))
(defvar nogood (car (day13/read-packets (list (elt problem 60)))))

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
      (setq result (let ((a (car left))
                         (b (car right)))
                     (setq left (rest left))
                     (setq right (rest right))
                     (cond
                      ((not (or a b)) '(nil t))
                      ((and a b) (list (day13/compare a b) nil))
                      ((not a) '(:ok nil))
                      ((not b) '(:error nil))
                      (t (error (format "Unexpected condition for '%s' vs '%s'" a b)))))))
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

(defun day13/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day13)
