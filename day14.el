;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 14 :example))
(defconst problem (advent/read-problem-lines 14 :problem))
(defconst day14/image-filename "day14.ppm")

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

(defun day14/add-bottom-line (grid value)
  (let ((last-line (aref grid (1- (length grid)))))
    (--dotimes (length last-line)
      (aset last-line it value))))

(defun day14/add-doom-line! (grid)
  (day14/add-bottom-line grid :DOOM))

(defun day14/create-normalized-playground (rock-def)
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
  (day14/basic-simulation!
   (setq *grid-data*
         (day14/create-normalized-playground
          (day14/read-problem lines)))))

(defun day14/add-bedrock-line! (grid)
  (day14/add-bottom-line grid :rock))

(defun day14/drop-if-possible! (grid-data)
  (let* ((grid (plist-get grid-data :grid))
         (source (plist-get grid-data :source)))
    (if (not (eq (advent/grid-get grid source) :sand))
        (progn
          (day14/drop-grain! grid-data)
          t))))

(defun day14/create-large-playground (rock-def)
  (let* ((segments (plist-get rock-def :segments))
         (limits (plist-get rock-def :limits))
         (max-depth (cdadr limits))
         (max-width (caadr limits)))
    ;; Add extra 2 rows to the bottom
    (let ((grid (advent/make-grid  (+ 3 max-depth) 1002 :void)))
      (day14/add-rock-to-grid! grid segments)
      (day14/add-bedrock-line! grid)
      (list :grid grid
            :source (let ((source (plist-get rock-def :source)))
                      (cons (cdr source) (car source)))))))

(defun day14/extended-simulation! (grid-data)
  (let ((counter 0))
    (while (day14/drop-if-possible! grid-data)
      (setq counter (1+ counter)))
    counter))

(defun day14/debug-write-current-grid-to-buffer ()
  "Print the result of the last simulation as a buffer (which may slow down Emacs considerably)"
  (advent/assert *grid-data* "No *grid-data* present. Did you run a simulation?")
  (let ((buffer (get-buffer-create "*Sand*"))
        (grid (plist-get *grid-data* :grid)))
    (with-current-buffer buffer
      (erase-buffer)
      (let ((row 0)
            (column 0))
        (-each (append grid nil)
          (lambda (line)
            (--each (append line nil)
              (insert (case (advent/grid-get grid (cons row column))
                        (:rock "#")
                        (:sand ".")
                        (:void " ")
                        (:DOOM "🡃")
                        (t (error (format "Unexpected value '%s' for (%d . %d)" (advent/grid-get grid (cons row column))
                                          row column)))))
              (setq column (1+ column)))
            (setq row (1+ row))
            (setq column 0)
            (insert "\n")))))
    (switch-to-buffer buffer)))

(defun day14/debug-write-current-grid-to-ppm (&optional filename)
  "Print the result of the last simulation to a ppm file"
  (advent/assert *grid-data* "No *grid-data* present. Did you run a simulation?")
  (setq filename (or filename day14/image-filename))
  (let* ((grid (plist-get *grid-data* :grid))
         (grid-size (advent/get-grid-size grid)))
   (with-temp-buffer
     (insert (format "P3\n%d %d\n255\n" (cdr grid-size) (car grid-size)))
     (let ((row 0)
           (column 0))
       (-each (append grid nil)
         (lambda (line)
           (--each (append line nil)
             (insert (case (advent/grid-get grid (cons row column))
                       (:rock "133 55 71 ")
                       (:sand "226 190 49 ")
                       (:void "190 190 190 ")
                       (:DOOM "255 255 0 ")
                       (t (error (format "Unexpected value '%s' for (%d . %d)" (advent/grid-get grid (cons row column))
                                         row column)))))
             (setq column (1+ column)))
           (setq row (1+ row))
           (setq column 0)
           (insert "\n"))))
     (write-file filename)
     filename)))

(defun day14/part-2 (lines)
  (day14/extended-simulation! (setq *grid-data* (day14/create-large-playground (day14/read-problem lines)))))

(provide 'day14)

