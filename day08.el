(require 'dash)
(require 'advent-utils)

(defun day08/rotate-coordinate (n x-y)
  (cons (- n (cdr x-y) 1) (car x-y)))

(defun day08/rotate-grid (grid)
  (let ((n (length grid))
        (new-grid (advent/copy-grid grid)))
    (loop for x from 0 below n do
          (loop for y from  0 below n do
                (advent/grid-set! new-grid
                                  (day08/rotate-coordinate n (cons x y))
                                  (advent/grid-get grid (cons x y)))))
    new-grid))

(defun day08/visibile-trees-in-row (row)
  (cadr
   (-reduce-from (lambda (max&list index-tree)
                   (seq-let (max visibiles) max&list
                     (let ((index (car index-tree))
                           (height (cdr index-tree)))
                       (if (> height max)
                           (list height (cons index visibiles))
                         max&list))))
                 (list -1 nil)
                 (--map-indexed (cons it-index it)
                                (append row nil)))))

(defun day08/visibile-tree-coordinates-for-row (grid row)
  (--map (cons row it) (day08/visibile-trees-in-row (aref grid row))))

(defun day08/all-visibile-coordinates (grid &optional visibile)
  (let ((n (length grid)))
    (loop for i from 0 below n do
          (setq visibile (append (day08/visibile-tree-coordinates-for-row grid i) visibile))))
  (list grid visibile))

(defun day08/rotate-problem (grid visibile)
  (list (day08/rotate-grid grid)
        (--map (day08/rotate-coordinate (length grid) it) visibile)))

(defun day08/raw-get-all-visibile-coordinates (grid)
  (apply #'day08/rotate-problem
         (apply #'day08/all-visibile-coordinates
          (apply #'day08/rotate-problem
                 (apply #'day08/all-visibile-coordinates
                        (apply #'day08/rotate-problem
                               (apply #'day08/all-visibile-coordinates
                                      (apply #'day08/rotate-problem
                                             (day08/all-visibile-coordinates grid)))))))))

(defun day08/get-all-visibile-coordinates (grid)
  (-uniq (--sort  (or (> (car it) (car other))
                      (and (= (car it) (car other))
                           (>= (cdr it) (cdr other))))
                  (cadr (day08/raw-get-all-visibile-coordinates grid)))))

(defun day08/part-1 (grid)
  (length (day08/get-all-visibile-coordinates grid)))

;;; TODO/FIXME does lisp support destructuring in function arguments?
(defun day08/right-scenic-score-for-tree (trees)
  (let ((first (car trees)))
    (car
     (-reduce-from (lambda (view&stop tree)
                     (seq-let (view stop) view&stop
                       (if stop
                           view&stop
                         (cond
                          ((>= tree first) (list (1+ view) t))
                          ((< tree first) (list (1+ view) nil))))))
                   (list 0 nil)
                   (rest trees)))))

(defun day08/compute-right-scenic-score (row)
  (-map #'day08/right-scenic-score-for-tree
        (-partition-all-in-steps (length row) 1 (append row nil))))

(defun day08/merge-scores! (row scores results)
  (let ((score-row (aref scores row)))
    (loop for column from 0 below (length scores) do
          (aset score-row column (* (aref score-row column) (elt results column)))))
  scores)

(defun day08/score-right! (grid &optional scores)
  (let* ((n (length grid))
        (scores (or scores (advent/make-grid n n 1))))
    (loop for row from 0 below n do
          (day08/merge-scores! row
                               scores
                               (day08/compute-right-scenic-score (aref grid row)) ))
    (list grid scores)))

(defun day08/rotate-score-problem (grid&score)
  (list (day08/rotate-grid (car grid&score))
        (day08/rotate-grid (cadr grid&score))))

(defun day08/compute-scenic-scores (grid)
  (cadr
   (day08/rotate-score-problem
    (apply #'day08/score-right!
           (day08/rotate-score-problem
            (apply #'day08/score-right!
                   (day08/rotate-score-problem
                    (apply #'day08/score-right!
                           (day08/rotate-score-problem
                            (day08/score-right! grid))))))))))

(defun day08/find-max-scenic-score (scores)
  (let ((max-score -1))
    (advent/-each-grid scores
      (when (> it-value max-score)
        (setq max-score it-value)))
    max-score))

(defun day08/compute-max-scenic-score (grid)
  (day08/find-max-scenic-score (day08/compute-scenic-scores grid)))

(defun day08/part-2 (grid)
  (day08/compute-max-scenic-score grid))

(provide 'day08)
