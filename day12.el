(require 'dash)
(require 'advent-utils)

(defconst example (advent/read-grid 12 :example #'identity))
(defconst problem (advent/read-grid 12 :problem #'identity))

(defconst day12/base-level (string-to-char "a"))
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
          (advent/grid-set! grid it-coord day12/base-level)
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
    ;; I'm saving the heights in the visited set
    (advent/-each-grid grid
      (if (eq (advent/get raw-set it-coord) :none)
          (advent/put raw-set it-coord it-value)))
    raw-set))

(defun day12/create-distances (grid-data)
  (let ((distances (advent/make-grid (day12/get-rows grid-data)
                                     (day12/get-columns grid-data)
                                     :inf)))
    (advent/grid-set! distances (plist-get grid-data :start) 0)
    distances))

(defun day12/get-grid-height (grid-data coord)
  (advent/grid-get (plist-get grid-data :grid) coord))

(defun day12/get-dji-height (d-data coord)
  (advent/get (plist-get d-data :unvisited) coord))

(defun day12/sum-coordinates (a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(defun day12/available-upwards-neighbors (d-data row&column)
  (let ((unvisited (plist-get d-data :unvisited))
        (height (plist-get d-data :height)))
    (--filter (let ((candidate-distance (day12/get-dji-height d-data it)))
                (>= height (1- candidate-distance)))
     (--filter it (-map (lambda (increment)
                          (let ((new-coord (day12/sum-coordinates increment row&column)))
                            (and (advent/get unvisited new-coord) new-coord)))
                        '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))))  )

(defun day12/available-downwards-neighbors (d-data row&column)
  (let ((unvisited (plist-get d-data :unvisited))
        (height (plist-get d-data :height)))
    (--filter (let ((candidate-distance (day12/get-dji-height d-data it)))
                (<= height (1+ candidate-distance)))
     (--filter it (-map (lambda (increment)
                          (let ((new-coord (day12/sum-coordinates increment row&column)))
                            (and (advent/get unvisited new-coord) new-coord)))
                        '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))))))


(defun day12/create-dijkstra-initial-data (grid-data)
  (let ((current-coord (plist-get grid-data :start)))
    (list :current current-coord
          :height (advent/grid-get (plist-get grid-data :grid) current-coord)
          :distances (day12/create-distances grid-data)
          :unvisited (day12/create-unvisited-set grid-data))))


(defun day12/get-unvisited-neighbors (grid-data d-data)
  (let ((current-coord (plist-get d-data :current))
        (current-height (plist-get d-data :height)))
    ))

(defun day12/get-finish-distance (grid-data d-data)
  (let ((raw-distance (advent/grid-get (plist-get d-data :distances)
                                       (plist-get grid-data :finish))))
    (unless (eq raw-distance :inf)
      raw-distance)))

(defun day12/update-distances! (distances current neighbors)
  (let ((current-distance (advent/grid-get distances current))
        (distances (plist-get d-data :distances)))
    (--each neighbors
      (let ((neighbor-distance (advent/grid-get distances it))
            (distance-candidate (1+ current-distance)))
        (if (or (eq :inf neighbor-distance)
                (> neighbor-distance distance-candidate))
            (advent/grid-set! distances it distance-candidate))))))

(defun day12/pick-unvisited-node (distances unvisited)
  (let ((all-coords (advent/map-hash unvisited (lambda (k _) k))))
    (car
     (first
     (--sort (< (cadr it)
                (cadr other))
             (--filter (not (eq (cadr it) :inf))
                       (--map (list it (advent/grid-get distances it))
                              all-coords)))))))

(defun day12/neighbors-present? (d-data)
  (let ((distances (plist-get d-data :distances))
        (unvisited (plist-get d-data :unvisited)))
   (--filter (not (eq (cadr it) :inf))
             (--map (list it (advent/grid-get distances it))
                    (advent/map-hash unvisited (lambda (k _) k))))))

;; TODO/FIXME cache, even horribly if required, this bad boy
(defun day12/dijkstra-should-continue? (grid-data d-data)
  (if-let ((finish (plist-get grid-data :finish)))
      ;; If finish is specified, stop as soon as I get there
      (eq (advent/grid-get (plist-get d-data :distances) finish) :inf)
    ;; otherwise stop as soon as the unvisited node is empty (slooooooowww!)
    (day12/neighbors-present? d-data)))

(defun day12/dijkstra (grid-data neighbors-f)
  (let ((d-data (day12/create-dijkstra-initial-data grid-data)))
    (while (day12/dijkstra-should-continue? grid-data d-data)
      (let ((current (plist-get d-data :current))
            (height (plist-get d-data :height))
            (distances (plist-get d-data :distances))
            (unvisited (plist-get d-data :unvisited)))
        (let ((available-neighbors (funcall neighbors-f d-data current)))
          (if (not available-neighbors)
              (progn
                (advent/delete unvisited current)
                (setq current (day12/pick-unvisited-node distances  unvisited)))
            (day12/update-distances! distances current available-neighbors)
            ;; Remove the current element from the list of unvisited
            (advent/delete unvisited current)
            ;; Select the new current element and cache its height
            (setq current (day12/pick-unvisited-node distances unvisited))
            (advent/assert current "Nil current detected!"))
          (setq d-data (list :current current
                             :height (advent/get unvisited current)
                             :distances distances
                             :unvisited unvisited)))))
    d-data))

(defun day12/part-1 (grid)
  (let* ((grid-data (day12/read-grid-data grid))
         (d-data (day12/dijkstra grid-data #'day12/available-upwards-neighbors)))
    (day12/get-finish-distance grid-data d-data)))

(defun day12/invert-problem (old-grid-data)
  (list :grid (plist-get old-grid-data :grid)
        :rows (plist-get old-grid-data :rows)
        :columns (plist-get old-grid-data :columns)
        :start (plist-get old-grid-data :finish)
        :finish nil))

(defun day12/find-all-starts (grid-data d-data)  
  (let* ((grid (plist-get grid-data :grid))
        (distances (plist-get d-data :distances))
        (candidate))
    (advent/-each-grid distances
      (unless (or (eq it-value :inf)
                  (/= (advent/grid-get grid it-coord) day12/base-level))
        (setq candidate (cons it-value candidate))))
    candidate))

(defun day12/part-2 (grid)
  (let* ((grid-data (day12/invert-problem (day12/read-grid-data grid)))
         (d-data (day12/dijkstra grid-data #'day12/available-downwards-neighbors)))
    (car (-sort #'< (day12/find-all-starts grid-data d-data)))))

(provide 'day12)

