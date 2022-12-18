(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 18 :example))
(defconst problem (advent/read-problem-lines 18 :problem))

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

(defun day18/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day18)
