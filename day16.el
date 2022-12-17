;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)
(require 'avl-tree)

(defconst example (advent/read-problem-lines 16 :example))
(defconst problem (advent/read-problem-lines 16 :problem))

(defconst day16/inf 1e6 "A distance large enough to be de-facto infinite for Dijkstra algorithm's sake")

(defconst day16/total-time 30)
(defconst day16/reduced-time 26)

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

(defvar *cached-dijkstra* nil)

(defun day16/get-dijkstra (valve-data start-name)
  (or (advent/get *cached-dijkstra* start-name)
      (progn
        (let ((new-dijkstra (day16/dijkstra valve-data start-name)))
          (advent/put *cached-dijkstra* start-name new-dijkstra)
          new-dijkstra))))


(defun day16/get-non-zero-flow-nodes (valve-data)
  (-map #'car (--filter (not (zerop (cdr it))) (advent/-map-hash valve-data (cons it-key (day16-valve-flow it-value))))))

(defun day16/assert-has-no-connections-to-flowing-tunnels (valve-data node-name)
  "Checks if there are connections between valves with non 0 flows.

It turns out it can happen"
  (--each (day16-valve-tunnels (advent/get valve-data node-name))
    (advent/assert (zerop (day16-valve-flow (advent/get valve-data it))))))



(defun day16/move-cost (valve-data start-name end-name)
  (let ((distances (day16/get-dijkstra valve-data start-name)))
    (advent/get distances end-name)))

(defun day16/move-outcome (valve-data time start-name end-name max-time)
  "Returns the finish time and outcome (valve throughput until 30) for the move"
  (let ((cost (1+ (day16/move-cost valve-data start-name end-name))))
    (list  (+ time cost)
           (max 0 (* (day16-valve-flow (advent/get valve-data end-name))
                     (- max-time (+ time cost)))))))

(defstruct day16-path "Path with time and total projected flow so far"
           path
           time
           max-time
           projected-flow
           remaining)

(defun day16/max-projected-outcome (valve-data path time max-time)  
  (let ((all-sorted-flows (-sort #'> (--map (day16-valve-flow (advent/get valve-data it))
                                            (day16-path-remaining path)))))
    (+ (day16-path-projected-flow path)
       (apply #'+ (--map (* (car it) (cdr it))
                         (--filter (> (car it) 0)
                                   (--map-indexed (cons (- max-time time (* (1+ it-index) 2))
                                                        it)
                                                  all-sorted-flows)))))))

(defun day16/path-outcome (valve-data current-path-data end-valve-name)
  (let ((previous-path (day16-path-path current-path-data))
        (remaining (day16-path-remaining current-path-data))
        (current-flow (day16-path-projected-flow current-path-data))
        (max-time (day16-path-max-time current-path-data)))
   (seq-let (end-time new-projected-flow) (day16/move-outcome valve-data
                                                          (day16-path-time current-path-data)
                                                          (car (day16-path-path current-path-data))
                                                          end-valve-name
                                                          max-time)
     (make-day16-path :path (cons end-valve-name previous-path)
                      :time end-time
                      :max-time max-time
                      :projected-flow (+ new-projected-flow current-flow)
                      :remaining (-remove-item end-valve-name remaining)))))

(defun day16/sub-paths (valve-data path-data)
  (--map (day16/path-outcome valve-data
                             path-data
                             it)
         (--sort (> (day16-valve-flow (advent/get valve-data it))
                    (day16-valve-flow (advent/get valve-data other)))
                 (day16-path-remaining path-data))))


;;; TODO/FIXME remove
(defvar *best-so-far* 0 "This is a horrible thing…" )

(defun day16/best-path-options (valve-data path-data)
  (let ((result (let* ((last-node (car (day16-path-path path-data)))
                       (distances (day16/get-dijkstra valve-data last-node))
                       (remaining (day16-path-remaining path-data))
                       (time (day16-path-time path-data))
                       (max-time (day16-path-max-time path-data)))
                  ;; I could probably add another condition for projected flow
                  (if (or (>= time max-time)
                          (not remaining)
                          (and t (<= (day16/max-projected-outcome valve-data path-data time max-time) *best-so-far*)))
                      (let ((result (day16-path-projected-flow path-data)))
                        (comment
                          (print (format "Result: %s (Best: %s)" result *best-so-far*))
                          (sit-for 0))
                        result)      
                    (let ((result (car
                                   (-sort #'>
                                          (--map (day16/best-path-options valve-data it)
                                                 (day16/sub-paths valve-data path-data))))))
                      result)))))
    (setq *best-so-far* (max result *best-so-far*))
    *best-so-far*))


(defun day16/best-path (valve-data)
  (setq *best-so-far* 0)
  (setq *cached-dijkstra* (make-hash-table))
  (day16/best-path-options valve-data
                           (make-day16-path :path '(:AA)
                                            :time 0
                                            :max-time day16/total-time
                                            :projected-flow 0
                                            :remaining (day16/get-non-zero-flow-nodes valve-data))))


(defun day16/part-1 (lines)
  (day16/best-path (day16/read-problem lines)))

(setq *best-sum-so-far* 0)

(defun day16/saving-result (result)
  (when (> result *best-sum-so-far*)
    (print (format "%d %s" *current-i* result))
    (sit-for 0)
    (setq *best-sum-so-far* result))
  *best-sum-so-far*)

(defun day16/max-projected-value (valve-data path)
  (let ((sum 0)
        (time (- day16/reduced-time 2)))
    (--each (-sort #'> (--map (day16-valve-flow (advent/get valve-data it)) path))
      (setq sum (+ sum (max 0 (* time it))))
      (setq time (- time 2)))
    sum))

(defun day16/best-paths (valve-data valves-a valves-b)
  (day16/saving-result
   (let ((projected-a (day16/max-projected-value valve-data valves-a))
         (projected-b (day16/max-projected-value valve-data valves-b)))
     (if (<= (+ projected-a projected-b) *best-sum-so-far*)
         *best-sum-so-far*
       (setq *best-so-far* 0)
       (let ((first-flow (day16/best-path-options valve-data
                                                  (make-day16-path :path '(:AA)
                                                                   :time 0
                                                                   :max-time day16/reduced-time
                                                                   :projected-flow 0
                                                                   :remaining valves-a))))
         (if (<= (+ first-flow projected-b) *best-sum-so-far*)
             *best-so-far*
           (setq *best-so-far* 0)
           (let ((second-flow (day16/best-path-options valve-data
                                                       (make-day16-path :path '(:AA)
                                                                        :time 0
                                                                        :max-time day16/reduced-time
                                                                        :projected-flow 0
                                                                        :remaining valves-b))))
             (+ first-flow second-flow))))))))

(defun day16/pad-to-bits (n list)
  (append (-repeat (- n (length list)) 0)
          list))

(defun day16/int-to-binary-list (bits i)
  (let ((res))
    (while (not (= i 0))
      (setq res (cons (if (= 1 (logand i 1)) 1 0) res))
      (setq i (lsh i -1)))
    (day16/pad-to-bits bits res)))

(defun day16/split-list (list number)
  (let ((masked (--zip-with (cons it other) (day16/int-to-binary-list (length list) number) list) ))
    (list (-map #'cdr (--filter (zerop (car it)) masked))
          (-map #'cdr (--filter (not (zerop (car it))) masked)))))

(defun day16/best-combination (valve-data)
  (setq *cached-dijkstra* (make-hash-table))  
  (let* ((non-zero-nodes (day16/get-non-zero-flow-nodes valve-data))
         (max-bit-value (expt 2 (length non-zero-nodes)))
         (max-value 0))
    (setq *current-i* 0)
    (setq *best-sum-so-far* (day16/best-paths valve-data
                                              (-take (/ (length non-zero-nodes) 2) non-zero-nodes)
                                              (-drop (/ (length non-zero-nodes) 2) non-zero-nodes)))
    (--dotimes max-bit-value
      (setq *current-i* it)
      (seq-let (list-a list-b) (day16/split-list non-zero-nodes it)
        (setq max-value (max max-value
                             (day16/best-paths valve-data
                                               list-a
                                               list-b)))))
    max-value))

(defun day16/part-2 (lines)
  (day16/best-combination (day16/read-problem lines)))

(provide 'day16)
