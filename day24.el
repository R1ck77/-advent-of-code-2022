(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 24 :example))
(defconst problem (advent/read-problem-lines 24 :problem))

(defun day24/char-to-grid (char)
  (cond
   ((string= char "#") :wall)
   ((string= char ".") :empty)
   ((string= char ">") '(0 . 1))
   ((string= char "<") '(0 . -1))
   ((string= char "v") '(1 . 0))
   ((string= char "^") '(-1 . 0))))

(defun day24/read-problem (lines)
  (let ((map (advent/table))
        (grid (advent/lines-to-grid lines #'day24/char-to-grid)))
    (advent/-each-grid grid
      (unless (eq it-value :empty)
        (advent/put map it-coord it-value)))
    (list :map map
          :size (advent/get-grid-size grid))))

(defun day24/grid-to-char (el)
  (cond
   ((eq el :wall) "#")
   ((eq el :empty) ".")
   ((equal el '(0 . 1)) ">")
   ((equal el '(0 . -1)) "<")
   ((equal el '(1 . 0)) "v")
   ((equal el '(-1 . 0)) "^")
   (t (error "Unexpected element"))))

(defun day24/debug-print-map (map-data)
  (let ((rows-columns (plist-get map-data :size))
        (map (plist-get map-data :map)))
    (with-current-buffer (get-buffer-create "*day24*")
      (erase-buffer)
      (-dotimes (car rows-columns)
        (lambda (row)
          (-dotimes (cdr rows-columns)
            (lambda (column)
              (insert
               (day24/grid-to-char
                (advent/get map (cons row column) :empty)))))
          (insert "\n")))
      (sit-for 0.01)
      (display-buffer (current-buffer)))))

(defun day24/sum-coord (a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(defun day24/wrap-blizzard (grid-size unwrapped-coord value)
  (let ((rows (car grid-size))
        (columns (cdr grid-size))
        (row (car unwrapped-coord))
        (column (cdr unwrapped-coord)))
    (cond
     ((equal value '(1 . 0)) (cons 1 column ))
     ((equal value '(-1 . 0)) (cons (- rows 2) column))
     ((equal value '(0 . 1)) (cons row 1))
     ((equal value '(0 . -1)) (cons row (- column 2))))))

(defun day24/evolve-element! (old-data new-map coord value)
  (let ((size (plist-get old-data :size))
        (old-map (plist-get old-data :map)))
   (if (eq value :wall)
       ;; Walls are unchanging
       (advent/put new-map coord value)
     (let ((new-coord (day24/sum-coord coord value)))
       (if (eq (advent/get old-map new-coord) :wall)
           (advent/put new-map (day24/wrap-blizzard size  new-coord value) value)
         (advent/put new-map new-coord value))))))

(defun day24/evolve-map (map-data)
  (let ((new-map (advent/table))
        (old-map (plist-get map-data :map)))
    (advent/each-hash old-map
      (lambda (key value) (day24/evolve-element! map-data
                                                 new-map
                                                 key value)))
    (list :map new-map
          :size (plist-get map-data :size))))

(setq e (day24/read-problem example))
(setq p (day24/read-problem problem))

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)
