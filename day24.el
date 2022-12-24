(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 24 :example))
(defconst problem (advent/read-problem-lines 24 :problem))

(defconst day24/starting-pos (cons 0 1))
(defconst day24/inf most-positive-fixnum)

(defun day24/char-to-grid (char)
  (cond
   ((string= char "#") :wall)
   ((string= char ".") :empty)
   ((string= char ">") '(0 . 1))
   ((string= char "<") '(0 . -1))
   ((string= char "v") '(1 . 0))
   ((string= char "^") '(-1 . 0))))

(defconst day24/moves (list '(0 . 0)
                            '(1 . 0)
                            '(-1 . 0)
                            '(0 . 1)
                            '(0 . -1)))

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
    (list day24/starting-pos
          (cons (1- (car size)) (- (cdr size) 2)))))

(defstruct day24-state "Status of the simulation"
           time
           pos)

(defun day24/create-starting-state ()
  (make-day24-state :time 0
                    :pos day24/starting-pos))

(defun day24/get-possible-moves (cache state)
  (let* ((now (day24-state-time state))
         (pos (day24-state-pos state))
         (next-data (day24/get-map cache (1+ now)))
         (next-map (plist-get next-data :map))
         (size (plist-get next-data :size)))
    (--filter (and (>= (car it) 0)
                   (< (car it) (car size)))
     (--filter (not (advent/get next-map it))
               (--map (day24/sum-coord it pos) day24/moves)))))

(defun day24/next-moves (cache state)
  (let ((possible-moves)
        (now (day24-state-time state)))
    (--map (make-day24-state :pos it
                             :time (1+ now))
           (day24/get-possible-moves cache state))))

(defun day24/evolve-simulation (map-data)
  "First breadth first approach"
  (let ((moves (list (day24/create-starting-state)))
        (end-position (cadr (day24/get-extremes map-data)))
        (cache (day24/create-cache map-data))
        (time 0)
        (stop))
    (while (not stop)
      (when (= (mod time 10) 9)
        (print (format "moves: %d (cache size=%d)" (length moves) (advent/table-size cache)))
        (sit-for 0.01))
      (setq moves (--mapcat (day24/next-moves cache it) moves))
      (when (--any (equal (day24-state-pos it) end-position) moves)
        (setq stop t))
      (setq time (1+ time)))
    time))

(defun day24/distance (state dest-pos)
  (let ((pos-a (day24-state-pos state)))
    (+ (abs (- (car pos-a) (car dest-pos)))
       (abs (- (cdr pos-a) (cdr dest-pos))))))

(defun day24/expand-nodes (cache nodes node)
  (let ((next-moves (day24/next-moves cache node)))
    (--each next-moves
            (advent/put nodes it 1))
      nodes))

(defun day24/create-available-nodes (cache)
  (let* ((starting-state (day24/create-starting-state))
        (nodes (advent/table))) ;;TODO/FIXME a balanced tree would be better
    (advent/put nodes starting-state 0)
    ;; Add the nearest neighbors
    (day24/expand-nodes cache nodes starting-state)))

(defun day24/minimize-f (f-time-state-a f-time-state-b)
  (seq-let (a-f a-time _) f-time-state-a
    (seq-let (b-f b-time _) f-time-state-b
      (or (< a-f b-f)
          (and (= a-f b-f)
               (< a-time b-time))))))

(defun day24/select-next-node (cache nodes destination-pos current-node)
  "Select the node that minimizes the distance from the target

Among all nodes with this specific, get the ones with the smallest time"
  (let* ((next-node)
        (next-s)
        (now (day24-state-time current-node))))
  (seq-let (f time state)
      (car (-sort #'day24/minimize-f
                  (--filter (not (equal (elt it 2) current-node))
                   (advent/map-hash nodes (lambda (state g)
                                            (list (+ g (day24/distance state destination-pos)) ; g + h
                                                  (day24-state-time state) ;time
                                                  state))))))
    state))

;; 435 too high
(defun day24/dijkstra (map-data)  
  (let* ((cache (day24/create-cache map-data))
         (nodes (day24/create-available-nodes cache))
         (current-node (day24/create-starting-state))
         (destination-pos (cadr (day24/get-extremes map-data)))
         (destination-reached nil)
         (next-node nil)
         (debug-last-sampling (time-to-seconds (current-time))))    
    (while (and (not destination-reached)
                (setq next-node (day24/select-next-node cache nodes destination-pos current-node)))
      (when-let ((now (time-to-seconds (current-time)))
                 (do-measure (> (- now debug-last-sampling) 1)))
        (setq debug-last-sampling now)        
        (print (format "next: %s Distance:%d" next-node (day24/distance next-node destination-pos)))
        (sit-for 0.01))
      
      (if (equal (day24-state-pos next-node) destination-pos)
          ;; Huzzah!
          (setq destination-reached t)
        ;; Expand the list of nodes with the neighbors of the next node
        (day24/expand-nodes cache nodes next-node)
        ;; Remove the current node from the list of available nodes
        (remhash current-node nodes)
        (setq current-node next-node)))
    ;; Return the time for reaching the destination
    (if destination-reached
        (day24-state-time next-node)
      (error "The algorithm failed =("))))



(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)
