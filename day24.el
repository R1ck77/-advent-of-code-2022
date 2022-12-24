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

(defun day24/add-to-map (map key value)
  (advent/update map key (lambda (key old-value)
                           (if (eq value :wall)
                               :wall
                             (cons value old-value)))))

(defun day24/read-problem (lines)
  (let ((map (advent/table))
        (grid (advent/lines-to-grid lines #'day24/char-to-grid)))
    (advent/-each-grid grid
      (unless (eq it-value :empty)
        (day24/add-to-map map it-coord it-value)))
    (list :map map
          :size (advent/get-grid-size grid))))

(defun day24/grid-to-char (el)
  (cond
   ((eq el :wall) "#")
   ((eq el :empty) ".")
   ((equal el '((0 . 1))) ">")
   ((equal el '((0 . -1))) "<")
   ((equal el '((1 . 0))) "v")
   ((equal el '((-1 . 0))) "^")
   ((<= (length el) 4) (number-to-string (length el)))
   (t (error "Unexpected value for the grid"))))

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
     ((equal value '(0 . -1)) (cons row (- columns 2))))))

(defun day24/evolve-element! (old-data new-map coord values)
  (let ((size (plist-get old-data :size))
        (old-map (plist-get old-data :map)))
   (if (eq value :wall)
       ;; Walls are unchanging
       (advent/put new-map coord value)     
     (let ((new-coords-value (--map (list (day24/sum-coord coord it) it) values)))
       (-each new-coords-value         
         (lambda (new-coord-value)
           (if (eq (advent/get old-map (car new-coord-value)) :wall)
               (day24/add-to-map new-map (day24/wrap-blizzard size
                                                              (car new-coord-value)
                                                              (cadr new-coord-value))
                                 (cadr new-coord-value))
             (day24/add-to-map new-map (car new-coord-value) (cadr new-coord-value)))))))))

(defun day24/evolve-map (map-data)
  (let ((new-map (advent/table))
        (old-map (plist-get map-data :map)))
    (advent/each-hash old-map
      (lambda (key value) (day24/evolve-element! map-data
                                                 new-map
                                                 key value)))
    (list :map new-map
          :size (plist-get map-data :size))))

(defun day24/create-cache (map-data)
  (let ((cache (make-hash-table)))
    (advent/put cache 0 map-data)))

(defun day24/get-map (cache day)
  (if-let ((cached-value (advent/get cache day)))
      cached-value
    (let ((previous-value (advent/get cache (1- day)))
          (new-value))
      (advent/assert previous-value "Did I skip a day? :|")
      (setq new-value (day24/evolve-map previous-value))
      (advent/put cache day new-value)
      new-value)))

(setq e (day24/read-problem example))
(setq p (day24/read-problem problem))

(defun day24/get-extremes (map-data)
  "Returns a list of start position and end position"
  (let ((size (plist-get map-data :size)))
    (list (cons 0 1)
          (cons (1- (car size)) (- (cdr size) 2)))))

(defstruct day24-state "Status of the simulation"
           time
           pos)

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)
