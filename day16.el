;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)
(require 'avl-tree)

(defconst example (advent/read-problem-lines 16 :example))
(defconst problem (advent/read-problem-lines 16 :problem))

(defconst day16/inf 1e6 "A distance large enough to be de-facto infinite for Dijkstra algorithm's sake")

(defstruct day16-valve "Valve definition"
           name
           flow
           tunnels)

(defun day16/read-tunnels (raw-tunnels)
  (--map (intern (s-append it ":")) (s-split ", " raw-tunnels)))

(defun day16/read-valve (line)
  (let ((matches (s-match "Valve \\(..\\) has flow rate=\\(.+\\); tunnels? leads? to valves? \\(.+\\)"
                          line)))
    (seq-let (_ name flow raw-tunnels) matches
      (make-day16-valve :name (intern (s-append name ":"))
                        :flow (string-to-number flow)
                        :tunnels (day16/read-tunnels raw-tunnels)))))

(defun day16/read-problem (lines)
  (let ((valves (make-hash-table)))
    (--each (-map #'day16/read-valve lines)
      (advent/put valves (day16-valve-name it) it))
    valves))

(defun day16/valves-comparator (distances valve-a valve-b)
  (let ((a-dist (advent/get distances valve-a))
        (b-dist (advent/get distances valve-b)))
   (if (= a-dist b-dist)
       (string< (symbol-name valve-a)
                (symbol-name valve-b))
     (< a-dist b-dist))))

(defun day16/make-distances (valve-data start-node)
  (let ((distances (make-hash-table)))
    (advent/-each-hash valve-data
      (advent/put distances it-key (if (eq it-key start-node) 0 day16/inf)))
    distances))

(defun day16/dijkstra-create-nodes (valve-data start-node)
  (let* ((distances (day16/make-distances valve-data start-node))
         (nodes (avl-tree-create (lambda (a b) (day16/valves-comparator distances a b )))))
    (advent/-each-hash valve-data
      (avl-tree-enter nodes it-key))
    (list nodes distances)))

(defun day16/dijkstra-next-node (nodes)
  (avl-tree-first nodes))

(defun day16/dijkstra-get-neighbors (valve-data distances current-node-name)
  (--map (cons (advent/get valve-data it)
               (advent/get distances it))
         (day16-valve-tunnels (advent/get valve-data current-node-name))))

(defun day16/dijkstra-update-distance! (nodes distances node distance-candidate)
  ;;; TODO/FIXME repetition
  (advent/assert (day16-valve-name node) "invalid node")
  (avl-tree-delete nodes (day16-valve-name node))
  (advent/put distances (day16-valve-name node) distance-candidate)
  (avl-tree-enter nodes (day16-valve-name node)))

(defun day16/dijkstra-remove-node! (nodes node-name)
  (avl-tree-delete nodes node-name))

(defun day16/dijkstra (valve-data start-node)
  (seq-let (nodes distances) (day16/dijkstra-create-nodes valve-data start-node)
    (let ((current-node-name nil)
          (neighbors))
      (while (setq current-node-name (day16/dijkstra-next-node nodes))
        (let ((current-distance (advent/get distances current-node-name)))
          (setq neighbors (day16/dijkstra-get-neighbors valve-data distances current-node-name))
          (-each neighbors
            (lambda (node&distance)
              (let ((distance-candidate (1+ current-distance)))                
                (if (< distance-candidate (cdr node&distance))
                    (day16/dijkstra-update-distance! nodes distances (car node&distance) distance-candidate))))))
        (day16/dijkstra-remove-node! nodes current-node-name)))
    distances))

(defun day16/get-non-zero-flow-nodes (valve-data)
  (-map #'car (--filter (not (zerop (cdr it))) (advent/-map-hash valve-data (cons it-key (day16-valve-flow it-value))))))

(defun day16/assert-has-no-connections-to-flowing-tunnels (valve-data node-name)
  "Checks if there are connections between valves with non 0 flows.

It turns out it can happen"
  (--each (day16-valve-tunnels (advent/get valve-data node-name))
    (advent/assert (zerop (day16-valve-flow (advent/get valve-data it))))))

(defun day16/part-1 (lines)
  (error "Not yet implemented"))

(defun day16/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day16)
