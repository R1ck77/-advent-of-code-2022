(require 'dash)
(require 'advent-utils)
(require 's)
(require 'avl-tree)

(defconst day18/displacements '((1 0 0)
                                (-1 0 0)
                                (0 1 0)
                                (0 -1 0)
                                (0 0 1)
                                (0 0 -1)))

(defun day18/read-line (line)
  (-map #'string-to-number (split-string line ",")))

(defun day18/read-problem (lines)
  (-map #'day18/read-line lines))

(defun day18/adjacent (a b)
  (= (apply #'+ (--map (abs (- (car it) (cdr it)) ) (-zip a b))) 1))

(defun day18/adjacent-indices (list index-a index-b)
  (day18/adjacent (elt list index-a)
                  (elt list index-b)))

(defun day18/compute-adjacency (list it-index)
  "Assume that the adjacency for i < it-index have been already accounted for"
  (let ((i (1+ it-index))
        (adjacencies 0))
    (while (and (< i (length list))
                (< adjacencies 6))
      (when (day18/adjacent-indices list it-index i )
        (setq adjacencies (1+ adjacencies)))
      (setq i (1+ i)))
    adjacencies))

(defun day18/compute-all-adjacencies (list)
  (let ((total-adjacencies 0))
    (--dotimes (length list)
      (setq total-adjacencies (+ total-adjacencies (day18/compute-adjacency list it))))
    total-adjacencies))

(defun day18/part-1 (lines)
  (let ((rocks (day18/read-problem lines)))
    (- (* 6 (length rocks))
       (* 2 (day18/compute-all-adjacencies rocks)))))

(defun day18/create-rocks-set (rocks)
  (let ((dict (advent/table)))
    (--each rocks
      (advent/put dict it t))
    dict))

(defun day18/f--a-b (f a b)
  (--map (funcall f (car it) (cdr it)) (-zip a b)))

(defun day18/valid-move? (rocks-set corners a)
  (seq-let (min max) corners
    (seq-let (x y z) a
      (and (>= x (1- (elt min 0))) (<= x (1+ (elt max 0)))
           (>= y (1- (elt min 1))) (<= y (1+ (elt max 1)))
           (>= z (1- (elt min 2))) (<= z (1+ (elt max 2)))))))

(defun day18/sum-lists (a b)
  (--map (+ (car it) (cdr it)) (-zip a b)))

(defun day18/valid-moves (rocks-set corners position)
  (--filter (day18/valid-move? rocks-set corners it)
            (--map (day18/sum-lists it position) day18/displacements)))


(defun day18/get-corners (rocks)
  (list (--reduce-from (day18/f--a-b #'min acc it) (car rocks) rocks)
        (--reduce-from (day18/f--a-b #'max acc it) (car rocks) rocks)))

(defun day18/hash-node (node)
  (+ (elt node 0)
     (* 1000 (elt node 1))
     (* 1000000 (elt node 2))))

(defun day18/nodes-comparator (node-a node-b)
  (seq-let (a score-a) node-a
    (seq-let (b score-b) node-b
      (cond
       ((and (not (eq score-a :inf)) (eq score-b :inf)) t)
       ((and (eq score-a :inf) (not (eq score-b :inf))) nil)
       ((eq score-a score-b) (< (day18/hash-node a)
                                (day18/hash-node b)))))))

(defun day18/create-nodes-set (corners rocks-set)
  (let ((tree (avl-tree-create #'day18/nodes-comparator)))
    (seq-let (min max) corners
      (-each (number-sequence (1- (elt min 0)) (1+ (elt max 0)))
        (lambda (x)
          (-each (number-sequence (1- (elt min 1)) (1+ (elt max 1)))
            (lambda (y)
              (-each (number-sequence (1- (elt min 2)) (1+ (elt max 2)))
                (lambda (z)
                  (let ((candidate (list x y z)))
                    (when (not (advent/get rocks-set candidate))
                      (avl-tree-enter tree (list candidate :inf))))))))))
      (let ((start (list (1- (elt min 0))
                         (1- (elt min 1))
                         (1- (elt min 2)))))
        (avl-tree-delete tree (list start :inf))
        (avl-tree-enter tree (list start  0))))
    tree))

(defun day18/next-move (nodes-set)
  (let ((candidate (avl-tree-first nodes-set)))
    (and (not (eq :inf (cadr candidate))) (car candidate))))

(defun day18/dijkstra (rocks)
  (let* ((corners (day18/get-corners rocks))
        (rocks-set (day18/create-rocks-set rocks))
        (nodes-set (day18/create-nodes-set corners rocks-set))
        (reachable-pockets (advent/table)))
    (let ((current))
      (while (setq current (day18/next-move nodes-set))
        (let ((reachable (day18/valid-moves rocks-set corners current)))
          (--each reachable
            (when (avl-tree-member nodes-set (list it :inf))
              (avl-tree-delete nodes-set (list it :inf))
              (avl-tree-enter nodes-set (list it 0))
              (advent/put reachable-pockets it t)))
          (avl-tree-delete nodes-set (list current 0)))))
    reachable-pockets))

(defun day18/exposed-sides (air-pockets rock)
  (length
   (--filter (advent/get air-pockets it)
             (--map (day18/sum-lists it rock) day18/displacements))))

(defun day18/sum-exposed-sides (rocks)
  (let ((air-pockets (day18/dijkstra rocks)))
    (--reduce-from (+ acc (day18/exposed-sides air-pockets it)) 0 rocks)))

(defun day18/part-2 (lines)
  (day18/sum-exposed-sides (day18/read-problem lines)))

(provide 'day18)
