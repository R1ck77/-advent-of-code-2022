(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-text 17 :example))
(defconst problem (advent/read-problem-text 17 :problem))

(defconst day17/well-width 7)
(defconst day17/well-slice (-repeat day17/well-width 0))
(defconst day17/rows-padding 3)

(defconst day17/debug-buffer "*Day17 well*")
(setq day17/debug-print-moves t) ;; TODO/FIXME constants

(defconst day17/tiles-bits '([[1 1 1 1]]
                             [[0 1 0]
                              [1 1 1]
                              [0 1 0]]
                             [[0 0 1]
                              [0 0 1]
                              [1 1 1]]
                             [[1]
                              [1]
                              [1]
                              [1]]
                             [[1 1]
                              [1 1]]))

;; TODO/FIXME make constants everywhere
(setq day17/tiles-masks '(((0 . 0) (0 . 1) (0 . 2) (0 . 3))
                          ((2 . 1)
                           (1 . 0) (1 . 1) (1 . 2)
                           (0 . 1))
                          ((2 . 0) (2 . 1) (2 . 2)
                           (1 . 2)
                           (0 . 2))
                          ((3 . 0)
                           (2 . 0)
                           (1 . 0)
                           (0 . 0))
                          ((1 . 0) (1 . 1)
                           (0 . 0) (0 . 1))))

(setq day17/all-tiles (-cycle (--zip-with (list it other)
                                              day17/tiles-bits
                                              day17/tiles-masks)))

(defun day17/read-problem (line)
  (--map (if (string= "<" it) -1 1) (split-string (string-trim line) "" t)))

(defun day17/generate-all-tiles (&optional count)
  (let ((repetitions (1+ (/ (or count 2022) (length day17/tiles)))))
    (-take count (-flatten (-repeat repetitions day17/tiles)))))

(defstruct day17-state "State of the simulation"
           base
           moves
           tiles)

(defstruct day17-tile "State of a tile. Pos is in row&column format, and starts always as (-1 . 2)"
           bits
           mask
           pos)

(defun day17/make-starting-state (moves)
  (make-day17-state :base nil
                    :moves (-cycle moves)
                    :tiles day17/all-tiles))

(defun day17/make-space-for-tile! (state)
  "Create enough room to simulate the tile (it basically adds three new lines…)

Assumes that the base is already trimmed to the height of the highest rock."
  (setf (day17-state-base state) (append (--map (apply #'vector it)
                                                (-repeat day17/rows-padding day17/well-slice))
                                         (day17-state-base state)))
  nil)

(defun day17/tile-out-of-well? (state tile)
  (let ((bits (day17-tile-bits tile))
        (row&column (day17-tile-pos tile)))
    (or (< (cdr row&column) 0)
        (> (+ (length (aref bits 0)) (cdr row&column)) day17/well-width)
        (>= (car row&column)
            (length (day17-state-base state))))))

(defun day17/-rock? (state pos)
  "Assumes that the coordinate is valid within the well"
  (not (zerop (aref (elt (day17-state-base state) (car pos)) (cdr pos)))))

(defun day17/-collides? (state tile)
  (let ((pos (day17-tile-pos tile))
        (mask (day17-tile-mask tile))
        (base (day17-state-base state)))
    (let ((collision))
     ;; start from the bottom of the tile to make this a bit faster
      (while (and (not collision) mask)
        (let* ((mask-el (car mask))
              (next-point (cons (+ (car mask-el) (car pos))
                                (+ (cdr mask-el) (cdr pos)))))
          (setq collision (day17/-rock? state next-point))
          (setq mask (rest mask))))
      collision)))

(defun day17/tile-clips-base? (state tile)
  (or (day17/tile-out-of-well? state tile)
      (let ((base (day17-state-base state))
            (tile-pos (day17-tile-pos tile)))
        (and (>= (car tile-pos) day17/rows-padding) ; The tile dropped enough
             (day17/-collides? state tile)))))

(defun day17/debug-number-to-char (value)
  (if (zerop value)
      " "
    "o"))

(defun day17/debug-print-base (state)
  (let ((base (day17-state-base state)))
    (with-current-buffer  (get-buffer-create day17/debug-buffer)
      (erase-buffer)
      (--each base
        (insert (format "#%s#\n" (apply #'concat(-map #'day17/debug-number-to-char it)))))
      (insert "#########")
      (goto-char (point-min))
      (display-buffer (current-buffer))))
  (sit-for 0)
  state)

(defun day17/extract-tile (state)
  (let ((tiles (day17-state-tiles state)))
    (list (make-day17-state :base (day17-state-base state)
                            :moves (day17-state-moves state)
                            :tiles (rest tiles))
          (make-day17-tile :bits (caar tiles)
                           :mask (cadr (car tiles))
                           :pos '(-1 . 2)))))

(defun day17/move-tile (tile dpos)  
  (let ((old-pos (day17-tile-pos tile))
        (new-tile (copy-day17-tile tile)))
       (setf (day17-tile-pos new-tile) (cons (+ (car old-pos) (car dpos))
                                             (+ (cdr old-pos) (cdr dpos))))
       new-tile))

(defun day17/embed-tile! (state tile)
  (let ((mask (day17-tile-mask tile))
        (pos (day17-tile-pos tile))
        (base (day17-state-base state)))
    (--each (--map (cons (+ (car pos) (car it))
                         (+ (cdr pos) (cdr it)))
                   mask)
      (aset (elt base (car it)) (cdr it) 1))))

(defun day17/prune-empty-rows! (state)
  (let ((base (day17-state-base state)))
    (setf (day17-state-base state) (--drop-while (equal it day17/well-slice) base))))

(defun day17/drop-tile! (state tile)
  "Modifies both the state and tile"
  (day17/make-space-for-tile! state)
  (let ((collision)
        (pos (day17-tile-pos tile))
        (first-move t))
    (while (not collision)
      (let ((moves (day17-state-moves state)))
        (setf (day17-state-moves state) (cdr moves))
        ;; Attempt to move the tile sideways: the first move is always permitted
        (print (car moves))
        (let ((new-tile (day17/move-tile tile (cons 0 (car moves)))))
          (unless (and (not first-move) (day17/tile-clips-base? state new-tile))
            (setq tile new-tile)))
        ;; Attempt to move the tile down
        (let ((new-tile (day17/move-tile tile '(1 . 0))))
          (if (or first-move (not (day17/tile-clips-base? state new-tile)))
              (setq tile new-tile)
            (day17/embed-tile! state tile)
            (setq collision t))))
      (setq first-move nil)))
  (day17/prune-empty-rows! state))

(defun day17/simulate-next-tile (state)
  (seq-let (next-state new-tile) (day17/extract-tile state)
    (day17/drop-tile! next-state new-tile)
    next-state))

(defun day17/part-1 (lines &optional repetitions)  
  (-reduce-from (lambda (state index)
                  (let ((result (day17/simulate-next-tile state)))
                    (when day17/debug-print-moves 
                      (day17/debug-print-base result))
                    result))
                 (day17/make-starting-state (day17/read-problem lines))
                 (number-sequence 0 (1- (or repetitions 2022)))))

(defun day17/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day17)
