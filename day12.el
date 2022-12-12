(require 'dash)
(require 'advent-utils)

(defconst example (advent/read-grid 12 :example #'identity))
(defconst problem (advent/read-grid 12 :problem #'identity))

(defconst day12/lower-level (string-to-char "a"))
(defconst day12/highest-level (string-to-char "z"))
(defconst day12/start-cell-value (string-to-char "S"))
(defconst day12/end-cell-value (string-to-char "E"))


(defun day12/letters-to-numbers-grid (letters-grid)
  (advent/update-grid! (advent/copy-grid letters-grid) #'string-to-char))

(defun day12/read-grid-data (letters-grid)
  (let ((grid (day12/letters-to-numbers-grid letters-grid))
        (start)
        (finish))
    (advent/-each-grid grid
      (cond
       ((= it-value day12/start-cell-value)
        (progn
          (setq start it-coord)
          (advent/grid-set! grid it-coord day12/lower-level)
          ))
       ((= it-value day12/end-cell-value)
        (progn
          (setq finish it-coord)
          (advent/grid-set! grid it-coord day12/highest-level)))))
    (list
     :grid grid
     :rows (length grid)
     :columns (length (aref grid 0))
     :start start
     :finish finish)))

(defun day12/get-rows (grid-data)
  (plist-get grid-data :rows))

(defun day12/get-columns (grid-data)
  (plist-get grid-data :columns))

(defun day12/get-size (grid-data)
  (cons (day12/get-rows grid-data)
        (day12/get-columns grid-data)))

(defun day12/create-all-nodes-list (grid-data)
  (let ((rows&columns (day12/get-size grid-data)))
    (--mapcat (-map (lambda (column)
                      (cons it column))
                    (number-sequence 0 (1- (cdr rows&columns))))
              (number-sequence 0 (1- (car rows&columns))))))

(defun day12/create-unvisited-set (grid-data)
  (let ((raw-set (advent/set-from (day12/create-all-nodes-list grid-data) :none))
        (start-coord (plist-get grid-data :start))
        (grid (plist-get grid-data :grid)))
    (advent/assert (eq :none (advent/get raw-set (plist-get grid-data :finish))))
    ;; I'm saving the heights in the visited set
    (advent/-each-grid grid
      (if (eq (advent/get raw-set it-coord) :none)
          (advent/put raw-set it-coord it-value)))
    (advent/delete raw-set start-coord)
    raw-set))

(defun day12/create-distances (grid-data)
  (let ((distances (advent/make-grid (day12/get-rows grid-data)
                                     (day12/get-columns grid-data)
                                     :inf)))
    (advent/grid-set! distances (plist-get grid-data :start) 0)
    distances))

(defun day12/create-dijkstra-initial-data (grid-data)
  (list :current (plist-get grid-data :start)
        :distances (day12/create-distances grid-data)
        :unvisited (day12/create-unvisited-set grid-data)))

(defun day12/get-unvisited-neighbors (grid-data d-data))

(defun day12/dijkstra (grid-data)
  (day12/create-dijkstra-initial-data grid-data)
  
  )

(defun day12/part-1 (grid)
  (day12/dijkstra (day12/read-grid-data grid)))

(defun day12/part-2 (grid)
  (error "Not yet implemented"))

(provide 'day12)
