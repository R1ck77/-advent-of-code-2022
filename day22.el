(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-blocks-of-lines 22 :example))
(defconst problem (advent/read-blocks-of-lines 22 :problem))

(defconst day22/empty 0)
(defconst day22/floor 1)
(defconst day22/wall 2)

(defun day22/take-next-movement (line)
  (let ((movement-size (length
                        (--take-while (and (>= it ?0)
                                          (<= it ?9))
                                      (append line nil)))))
    (list (list (string-to-number (substring line 0 movement-size)) nil)
          (substring line movement-size))))

(defun day22/take-next-turn (line)
  (let ((s-movement (substring line 0 1))
        (remaining (substring line 1)))
    (list (list nil (cond
                   ((string= s-movement "R") 1)
                   ((string= s-movement "L") -1)
                   (t (error "Unexpected rotation"))))
          remaining)))

(defun day22/read-moves (lines)
  (let ((s-commands (car lines))
        (commands))
    (while (> (length s-commands) 0)
      (seq-let (next-command remaining)
          ;; If let?
          (let ((command-start (substring s-commands 0 1)))
            (if (or (string= "R" command-start)
                    (string= "L" command-start))
                (day22/take-next-turn s-commands)
              (day22/take-next-movement s-commands)))
           (setq commands (cons next-command commands))
           (setq s-commands remaining)))
    (nreverse commands)))

(defun day22/read-value (line column)
  (case (string-to-char (substring line column (1+ column)))
    (?  day22/empty)
    (?. day22/floor)
    (?# day22/wall)
    (t (error "Unexpected floor tile"))))

(defun day22/read-board (lines)
  (let* ((rows (length lines))
         (max-columns (apply #'max (-map #'length lines)))
         (board (advent/make-grid rows max-columns day22/empty)))
    (-each-indexed lines
      (lambda (row line)
        (--dotimes max-columns
          (if (< it (length line))
              (advent/grid-set! board
                                (cons row it)
                                (day22/read-value line it))))))
    board))

(defun day22/read-problem (blocks)
  (let ((board (day22/read-board (car blocks))))
    (list :board board
          :moves (day22/read-moves (cadr blocks)))))

(defun day22/create-global-data () 
  (setq e (day22/read-problem example))
  (setq p (day22/read-problem problem)))

(defun day22/row-as-list (problem-data row)
  (append (elt (plist-get problem-data :board) row) nil))

(defun day22/get-start-coordinates (problem-data)
  (cons 0
        (car (--first (= (cdr it) day22/floor)
                  (--map-indexed (cons it-index it)
                                 (day22/row-as-list problem-data 0))))))

(defstruct day22-state "State of the simulation"
           board
           pos
           direction
           moves)

(defun day22/create-initial-state (problem-data)
  (make-day22-state :board (plist-get problem-data :board)
                    :pos (day22/get-start-coordinates problem-data)
                    :direction 0
                    :moves (plist-get problem-data :moves)))

(defun day22/rotate (direction rotation)
  (mod (+ direction rotation) 4))

(defun day22/rotate-state (state)
  (let ((moves (day22-state-moves state)))
    (make-day22-state :board (day22-state-board state)
                      :pos (day22-state-pos state)
                      :direction (day22/rotate (day22-state-direction state)
                                               (cadar moves))
                      :moves (cdr moves))))

(defun day22/direction-to-displacement (direction)
  (case direction
    (0 '(1 . 0))
    (1 '(0 . -1))
    (2 '(-1 . 0))
    (3 '(0 . 1))))

(defun day22/get-tile (board position)
  (advent/grid-get board position))

(defun day22/first-non-floor-column (board row)
  (let ((column -1)
        (tile day22/empty))
    (while (= tile day22/empty)
      (setq column (1+ column))
      (setq tile (day22/get-tile board (cons row column))))
    (advent/assert (/= tile day22/empty))
    column))

(defun day22/next-right-coordinate (board position)
  (let ((columns (length (elt board 0)))
        (row (car position))
        (column (cdr position)))
    (let ((next-column (1+ column)))
      (cons row
            (if (or (= next-column columns)
                    (= (day22/get-tile board (cons row next-column))
                       day22/empty))
                (day22/first-non-floor-column board row)
              next-column
              )))))

(defun day22/last-non-floor-column (board row)
  (let ((column (length (aref board 0)))
        (tile day22/empty))
    (while (= tile day22/empty)
      (setq column (1- column))
      (setq tile (day22/get-tile board (cons row column))))
    (advent/assert (/= tile day22/empty))
    column))


(defun day22/next-left-coordinate (board position)
  (let ((columns (length (elt board 0)))
        (row (car position))
        (column (cdr position)))
    (let ((next-column (1- column)))
      (cons row
            (if (or (= next-column -1)
                    (= (day22/get-tile board (cons row next-column))
                       day22/empty))
                (day22/last-non-floor-column board row)
              next-column)))))

(defun day22/first-non-floor-row (board column)
  (let ((row -1)
        (tile day22/empty))
    (while (= tile day22/empty)
      (setq row (1+ row))
      (setq tile (day22/get-tile board (cons row column))))
    (advent/assert (/= tile day22/empty))
    row))

(defun day22/next-down-coordinate (board position)
  (let ((rows (length board))
        (row (car position))
        (column (cdr position)))
    (let ((next-row (1+ row)))
      (cons (if (or (= next-row rows)
                    (= (day22/get-tile board (cons next-row column))
                       day22/empty))
                (day22/first-non-floor-row board column)
              next-row)
            column))))

(defun day22/last-non-floor-row (board column)
  (let ((row (length board))
        (tile day22/empty))
    (while (= tile day22/empty)
      (setq row (1- row))
      (setq tile (day22/get-tile board (cons row column))))
    (advent/assert (/= tile day22/empty))
    row))

(defun day22/next-up-coordinate (board position)
  (let ((rows (length board))
        (row (car position))
        (column (cdr position)))
    (let ((next-row (1- row)))
      (cons (if (or (= next-row -1)
                    (= (day22/get-tile board (cons next-row column))
                       day22/empty))
                (day22/last-non-floor-row board column)
              next-row)
            column))))

(defun day22/get-next-tile-coordinate (board position direction)
  (case direction
    (0 (day22/next-right-coordinate board position))
    (1 (day22/next-down-coordinate board position))
    (2 (day22/next-left-coordinate board position))
    (3 (day22/next-up-coordinate board position))
    (t (error "Unexpected direction"))))

(defvar *day22/displacements-cache* (advent/table))

;; TODO/FIXME Not yet used
(defun day22/get-next-tile-coordinate-cached (board position direction)
  (if-let ((cached-next-tile-coordinate (advent/get *day22/displacements-cache* (list position
                                                                                      direction))))
      cached-next-tile-coordinate
    (let ((next-tile-coordinate (day22/get-next-tile-coordinate board position direction)))
      (advent/put *day22/displacements-cache*
                  (list position direction)
                  next-tile-coordinate)
      next-tile-coordinate)))

(defun day22/new-position (board position direction)
  "Returns the new position or nil in case of wall"
  (let* ((move (day22/get-next-tile-coordinate board position direction))
         (tile (day22/get-tile board move)))
    (cond 
     ((= day22/empty tile)
      (error "Unexpected empty tile"))
     ((= day22/floor tile)
      move)
     ((= day22/wall tile)
      nil))))

(defun day22/translate (board position direction translation)
  (let ((steps 0)
        (stop))
    (while (and (not stop) (< steps translation )) 
      (if-let ((new-position (day22/new-position board position direction)))
          (setq position new-position)
        (setq stop t))
      (setq steps (1+ steps)))
    position))

(defun day22/translate-state (state)
  (let ((board (day22-state-board state))
        (direction (day22-state-direction state))
        (moves (day22-state-moves state)))
   (make-day22-state :board board
                     :pos (day22/translate board
                                           (day22-state-pos state)
                                           direction
                                           (caar moves))
                     :direction direction
                     :moves (cdr moves))))

(defun day22/move (state)
  (let ((moves (day22-state-moves state)))
    (if (car (car moves))
        (day22/translate-state state)
      (day22/rotate-state state))))

(defun day22/debug-print-pos (state)
  (print (format "pos: %s dir: %s moves: %s"
                 (day22-state-pos state)
                 (day22-state-direction state)
                 (day22-state-moves state))))

(defun day22/debug-print-state (state)
  (with-current-buffer (get-buffer-create "*Map*")
    (erase-buffer)
    (-each (append (day22-state-board state) nil)
      (lambda (row)
        (--each (append row nil)
          (insert (cond 
                   ((= it day22/empty) " ")
                   ((= it day22/floor) ".")
                   ((= it day22/wall) "#"))))
        (insert "\n")))
    (let ((pos (day22-state-pos state))
          (direction (day22-state-direction state)))
      (goto-line (1+ (car pos)))
      (goto-char (+ (point) (cdr pos)))
      (delete-char 1)
      (insert (case direction
                (0 ">")
                (1 "v")
                (2 "<")
                (3 "^"))))
    (display-buffer "*Map*")
    (sit-for 0.1)
;    (read-from-minibuffer "Continue?")
    ))

(defun day22/consume-moves (state)
  (setq *day22/displacements-cache* (advent/table))
  ;(day22/debug-print-state state)
  (while (day22-state-moves state)
    (setq state (day22/move state))
    ;(day22/debug-print-state state)
    )
  state)

(defun day22/compute-password (state)
  (let ((pos (day22-state-pos  state))
        (direction (day22-state-direction state)))
    (+ (* (1+ (car pos)) 1000)
       (* (1+ (cdr pos)) 4)
       direction)))

(defun day22/part-1 (line-blocks)
  (day22/compute-password (day22/consume-moves (day22/create-initial-state (day22/read-problem line-blocks)))))

(defun day22/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day22)
