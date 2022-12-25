;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)
(require 'avl-tree)

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
   (if (eq values :wall)
       ;; Walls are unchanging
       (advent/put new-map coord values)     
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

(defun day24/distance (state dest-pos)
  (let ((pos-a (day24-state-pos state)))
    (+ (abs (- (car pos-a) (car dest-pos)))
       (abs (- (cdr pos-a) (cdr dest-pos))))))

(defun day24/expand-nodes (cache nodes-tree node)
  (let ((next-moves (day24/next-moves cache node)))
    (--each next-moves
      (avl-tree-enter nodes-tree it))
    nodes-tree))

(defun day24/hash (state-a)
  (let ((pos (day24-state-pos state-a)))
    (+ (car pos) (* 1000000 (cdr pos)))))

(defun day24/create-avl-tree-comparator (end-pos)
  (lambda (state-a state-b)
    (let ((sum-a (+ (day24/distance state-a end-pos) (day24-state-time state-a)))
          (sum-b (+ (day24/distance state-b end-pos) (day24-state-time state-b))))
      (or (< sum-a sum-b)
          (and (= sum-a sum-b)
               (< (day24/hash state-a)
                  (day24/hash state-b)))))))

(defun day24/create-available-nodes (cache starting-state end-pos)
  (let* ((nodes (avl-tree-create (day24/create-avl-tree-comparator end-pos))))
    ;; Add the nearest neighbors
    (day24/expand-nodes cache nodes starting-state)))

(defun day24/minimize-f (f-time-state-a f-time-state-b)
  (seq-let (a-f a-time _) f-time-state-a
    (seq-let (b-f b-time _) f-time-state-b
      (or (< a-f b-f)
          (and (= a-f b-f)
               (< a-time b-time))))))

(defun day24/select-next-node (nodes-tree current-node)
  "Select the node that minimizes the distance from the target

Among all nodes with this specific, get the ones with the smallest time"
  (avl-tree-delete nodes-tree current-node)
  (avl-tree-first nodes-tree))

(defun day24/update-nodes (cache nodes-tree node)
  (--each (day24/next-moves cache node)
    (avl-tree-enter nodes-tree it)))

(defun day24/one-path-dijkstra (map-data &optional verbose)
  (let ((cache (day24/create-cache map-data)))
   (day24-state-time
    (day24/dijkstra cache
                    (day24/create-starting-state)
                    (cadr (day24/get-extremes map-data))
                    verbose))))

(defun day24/dijkstra (cache start-state destination-pos &optional verbose)
  (let* ((nodes-tree (day24/create-available-nodes cache start-state destination-pos))
         (current-node start-state)
         (destination-reached nil)
         (next-node nil)
         (debug-last-sampling (time-to-seconds (current-time))))    
    (while (and (not destination-reached)
                (setq next-node (day24/select-next-node nodes-tree current-node)))
      (when-let ((_ verbose)
                 (now (time-to-seconds (current-time)))
                 (do-measure (> (- now debug-last-sampling) 1)))
        (setq debug-last-sampling now)        
        (print (format "next: %s Distance:%d" next-node (day24/distance next-node destination-pos)))
        (sit-for 0.01))
      
      (if (equal (day24-state-pos next-node) destination-pos)
          ;; Huzzah!
          (setq destination-reached t)
        ;; Expand the list of nodes with the neighbors of the next node
        (day24/update-nodes cache nodes-tree next-node)
        ;; Remove the current node from the list of available nodes
        (avl-tree-delete nodes-tree current-node)
        (setq current-node next-node)))
    ;; Return the time for reaching the destination
    (if destination-reached
        next-node
      (error "The algorithm failed =("))))

(defun day24/part-1 (lines)
  (day24/one-path-dijkstra (day24/read-problem lines)))

(defun day24/forgetful-elf-dijkstra (map-data)
  (let ((cache (day24/create-cache map-data))
        (start-end (day24/get-extremes map-data)))
    (let* ((end-state (day24/dijkstra cache
                                      (day24/create-starting-state)
                                      (cadr start-end)))
           (back-to-start-state (day24/dijkstra cache
                                                end-state
                                                (car start-end)))
           (back-to-end-state (day24/dijkstra cache
                                              back-to-start-state
                                              (cadr start-end))))
      (day24-state-time back-to-end-state))))

(defun day24/part-2 (lines)
  (day24/forgetful-elf-dijkstra (day24/read-problem lines)))

(provide 'day24)
