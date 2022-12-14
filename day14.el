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
  (cons (- (car point) dx) (cdr point)))

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

(defun day14/create-playground (rock-def)
  (setq rock-def (day14/normalize rock-def))
  (let ((segments (plist-get rock-def :segments))
        (limits (plist-get rock-def :limits)))
    (let ((grid (advent/make-grid  (1+ (cdadr limits)) (1+ (caadr limits)) :void)))
      (day14/add-rock-to-grid! grid segments)
      grid
      )))

(defun day14/part-1 (lines)
  (error "Not yet implemented"))

(defun day14/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day14)
