(require 'dash)
(require 'advent-utils)

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

(defun day09/compute-displacement (head tail)
  (day09/binary-op head tail #'-))

(defun day09/compute-coordinate-move (x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   ((= x 0) 0)))

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

(defun day09/move-tail-segment (moved-tails current-tail)
  (let* ((current-head (car moved-tails))         
         (new-tail (day09/move-tail current-head current-tail)))
    (cons new-tail moved-tails)))

(defun day09/move-head (move head)
  (case move
    (:right (cons (1+ (car head)) (cdr head)))
    (:left(cons (1- (car head)) (cdr head)))
    (:up (cons (car head) (1+ (cdr head))))
    (:down (cons (car head) (1- (cdr head))))))

(defun day09/move-tails (rope)
  (let ((raw-result (-reduce-from #'day09/move-tail-segment
                                  (list (car rope))
                                  (rest rope))))
    (reverse raw-result)))

(defun day09/evolve-coordinates (move rope)
  (day09/move-tails (cons (day09/move-head move (car rope))
                          (rest rope))))

(defun day09/do-step (state move)
  (let* ((rope (plist-get state :rope))
         (new-rope (day09/evolve-coordinates move rope)))
    (list :rope new-rope
          :positions (cons new-rope (plist-get state :positions)))))

(defun day09/run-moves (moves rope)
  (plist-get (-reduce-from #'day09/do-step
                           (list :rope rope
                                 :positions (list rope))
                           moves)
             :positions))

(defun day09/compute-unique-tail-positions (acc)
  (-uniq (-map #'car (-map #'last acc))))

(defun day09/part-1 (lines)
  (length
   (day09/compute-unique-tail-positions
    (day09/run-moves (day09/read-problem lines)
                     (-repeat 2 '(0 . 0))))))

(defun day09/part-2 (lines)
  (length
   (day09/compute-unique-tail-positions
    (day09/run-moves (day09/read-problem lines)
                     (-repeat 10 '(0 . 0))))))

(provide 'day09)
