(require 'dash)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 9 :example))
(defvar problem (advent/read-problem-lines 9 :problem))

(defconst day09/string-to-direction '(("R" . :right)
                                      ("L" . :left)
                                      ("U" . :up)
                                      ("D" . :down)))

(defun day09/read-line (line)
  (seq-let (string-dir count) (split-string line)
    (-repeat (string-to-number count)
             (cdr (assoc string-dir day09/string-to-direction)))))

(defun day09/read-problem (lines)
  (-mapcat #'day09/read-line lines))

(defun day09/binary-op (head tail f)
  (cons (funcall f (car head) (car tail))
        (funcall f (cdr head) (cdr tail))))

(defun day09/compute-displacement (head tail )
  (day09/binary-op head tail #'-))

(defun day09/compute-coordinate-move (x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   ((= x 0) 0)))

;;6521 too low
(defun day09/compute-correction (displacement)
  (cons (day09/compute-coordinate-move (car displacement))
        (day09/compute-coordinate-move (cdr displacement))))

(defun day09/add-correction (tail displacement)
  (day09/binary-op tail displacement #'+))

(defun day09/move-tail (head tail)
  (let ((displacement (day09/compute-displacement head tail)))
    (if (and (< (abs (car displacement)) 2)
             (< (abs (cdr displacement)) 2))
        tail
      (day09/add-correction tail (day09/compute-correction displacement)))))

(defun day09/move-head (move head)
  (case move
    (:right (cons (1+ (car head)) (cdr head)))
    (:left(cons (1- (car head)) (cdr head)))
    (:up (cons (car head) (1+ (cdr head))))
    (:down (cons (car head) (1- (cdr head))))))

(defun day09/evolve-coordinates (move head tail)
  (let* ((new-head (day09/move-head move head))
        (new-tail (day09/move-tail new-head tail)))
    ;(print (format "%s from H%s T%s to H%s T%s" move head tail new-head new-tail))
    (list new-head new-tail)))

(defun day09/valid-state? (head tail)
  (let ((displacement (day09/compute-displacement head tail)))
    (and (< (abs(car displacement)) 2)
         (< (abs (cdr displacement)) 2))))

(defun day09/do-step (state move)
  (let* ((head (plist-get state :head))
         (tail (plist-get state :tail))
         (new-coordinates (day09/evolve-coordinates move head tail)))
    (advent/assert (apply #'day09/valid-state? new-coordinates) (format "I found myself in a strange predicament (%s after %s from h%s t%s)" new-coordinates move head tail))
    (list :head (car new-coordinates)
          :tail (cadr new-coordinates)
          :positions (cons new-coordinates (plist-get state :positions)))))

(defun day09/run-moves (moves)
  (plist-get (-reduce-from #'day09/do-step
                 (list :head '(0 . 0)
                       :tail '(0 . 0)
                       :positions '('(0 . 0) '(0 . 0)))
                 moves)
             :positions))

(defun day09/compute-unique-tail-positions (acc)
  (-uniq (-map #'cadr acc)))

(defun day09/part-1 (lines)
  (length
   (day09/compute-unique-tail-positions
    (day09/run-moves
     (day09/read-problem lines)))))

(defun day09/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day09)
