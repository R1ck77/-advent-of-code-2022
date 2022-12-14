;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 14 :example))
(defconst problem (advent/read-problem-lines 14 :problem))

(defun day14/list-to-cons (a-b)
  (cons (car a-b) (cadr a-b)))

(defun day14/read-checkpoints (line)
  (--map (day14/list-to-cons (-map #'string-to-number (s-split "," it))) (s-split " -> " line)))

(defun day14/read-path (line)
  (-partition-in-steps 2 1 (day14/read-checkpoints line)))

(defun day14/read-path-segments (lines)
  (-flatten-n 1 (-map #'day14/read-path lines)))

(defun day14/read-bedrock-limits (segments)
  (let ((points (-uniq (-flatten segments))))
    (let ((min-x (caar points))
          (max-x (caar points))
          (min-depth (cdr (car points)))
          (max-depth (cdr (car points))))
      (--each points
        (let ((x (car it))
              (depth (cdr it)))
          (setq min-x (if (< x min-x) x min-x))
          (setq max-x (if (> x max-x) x max-x))
          (setq min-depth (if (< depth min-depth) depth min-depth))
          (setq max-depth (if (> depth max-depth) depth max-depth))))
      (list (cons min-x min-depth) (cons max-x max-depth)))))

(defun day14/read-problem (lines)
  (let ((segments (day14/read-path-segments lines)))
    (list :segments segments
          :limits (day14/read-bedrock-limits segments)
          :source '(500 . 0))))

(defun day14/normalize-point (dx point)
  (cons (1+ (- (car point) dx)) (cdr point)))

(defun day14/normalize-segment (dx path)
  (list (day14/normalize-point dx (car path))
        (day14/normalize-point dx (cadr path))))

(defun day14/normalize (rock-def)
  (let ((segments (plist-get rock-def :segments))
        (limits (plist-get rock-def :limits))
        (source (plist-get rock-def :source)))
    (let ((dx (caar limits)))
      (list :segments (--map (day14/normalize-segment dx it) segments)
            :limits (day14/normalize-segment dx limits)
            :source (day14/normalize-point dx source)))))

(defun day14/sequence (from to)
  (if (<= to from)
      (number-sequence to from)
    (number-sequence from to)))

(defun day14/add-rock-segment-to-grid! (grid a-b)
  (seq-let (a b) a-b
    (if (= (car a) (car b))
        ;; vertical line
        (--each (day14/sequence (cdr a) (cdr b))
          (advent/grid-set! grid (cons it (car a)) :rock))
      ;; horizontal line
      (--each (day14/sequence (car a) (car b))
        (advent/grid-set! grid (cons (cdr a) it) :rock)))))

(defun day14/add-rock-to-grid! (grid segments)
  (--map (day14/add-rock-segment-to-grid! grid it) segments))

(defun day14/add-doom-line! (grid)
  (let ((last-line (aref grid (1- (length grid)))))
    (--dotimes (length last-line)
      (aset last-line it :DOOM))))

(defun day14/create-playground (rock-def)
  (setq rock-def (day14/normalize rock-def))
  (let ((segments (plist-get rock-def :segments))
        (limits (plist-get rock-def :limits)))
    ;; Add extra lines/rows to the grid so I can more easily detect out-of grid events
    (let ((grid (advent/make-grid  (+ 2 (cdadr limits)) (+ 2 (caadr limits)) :void)))
      (day14/add-rock-to-grid! grid segments)
      (day14/add-doom-line! grid)
      (list :grid grid
            :source (let ((source (plist-get rock-def :source)))
                      (cons (cdr source) (car source)))))))

;; TODO/FIXME maybe I don't need this
(defun day14/generate-off-grid?-function (grid)
  (let ((max-x (length (aref grid 0)))
                (max-depth (length grid)))
   (lambda (pos)
     (let ((x (car pos))
           (depth (cdr pos)))
       (or (< x 0) (> x max-x)
           (> depth max-depth))))))

(defun day14/evaluate-move (grid pos dy&dx)
  (let* ((new-pos (cons (+ (car pos) (car dy&dx))
                        (+ (cdr pos) (cdr dy&dx))))
         (destination (condition-case nil (advent/grid-get grid new-pos) (error :out-of-bounds))))
    (case destination
      (:out-of-bounds (list :DOOM new-pos))
      (:DOOM (list destination new-pos))
      (:void (list destination new-pos))
      (:rock  nil)
      (:sand nil)      
      (t (error "Unexpected cell found")))))

(defun day14/get-grain-move (grid pos)
  (or (day14/evaluate-move grid pos '(1 . 0))
      (day14/evaluate-move grid pos '(1 . -1))
      (day14/evaluate-move grid pos '(1 . 1))))

;; TODO/FIXME very slow: I need to keep track of the pile heights
(defun day14/evolve-grain! (grid  pos)
  (let ((move (day14/get-grain-move grid pos)))
    (cond
     ((null move) (progn (advent/grid-set! grid pos :sand) :rest))
     ((eq :void (car move)) (day14/evolve-grain! grid (cadr move)))
     ((eq :DOOM (car move)) :stop)
     (t (error "Unexpected move found")))))

(defun day14/drop-grain! (grid-data)
  (let* ((grid (plist-get grid-data :grid))
        (source (plist-get grid-data :source)))
    (day14/evolve-grain! grid source)))

(defun day14/basic-simulation! (grid-data)
  (let ((counter 0))
    (while (not (eq (day14/drop-grain! grid-data) :stop))
      (setq counter (1+ counter)))
    counter))

(defun day14/part-1 (lines)
  (day14/basic-simulation! (day14/create-playground (day14/read-problem lines))))

(defun day14/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day14)
