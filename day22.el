(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-blocks-of-lines 22 :example))
(defconst problem (advent/read-blocks-of-lines 22 :problem))

(defconst day22/empty 0)
(defconst day22/floor 1)
(defconst day22/wall 2)

(defconst day22/right 0)
(defconst day22/down 1)
(defconst day22/left 2)
(defconst day22/up 3)

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
;;    (sit-for 0.1)
    (read-from-minibuffer "Continue?")
    ))

(defun day22/consume-moves (state)
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
  (day22/compute-password
   (day22/consume-moves
    (day22/create-initial-state
     (day22/read-problem line-blocks)))))

(defun day22/on-face (side pos corner)
  (let ((row (car pos))
        (column (cdr pos))
        (min-row (car corner))
        (min-column (cdr corner)))
    (and (>= row min-row) (>= column min-column)
         (< row (+ side min-row)) (< column (+ side min-column)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example specific functions

(defun day22/get-M-example (board)
  (/ (length board) 3))

(defun day22/get-face-start-corner-example (M index)
  (elt (list
        nil
        (cons 0 (* 2 M))
        (cons M 0)
        (cons M M) 
        (cons M (* 2 M)) 
        (cons (* 2 M) (* 2 M)) 
        (cons (* 2 M) (* 3 M)) )
       index))

;; The last form should be fed the local coordinates for the starting face modulo M
(setq day22/example-topology `(nil            ;we start from 1
                         ;; 1
                         (:right (6 ,day22/left (list (- M row 1) (1- M)))
                          :down (4 ,day22/down (list 0 column))
                          :left (3 ,day22/down (list 0 row))
                          :up (2 ,day22/down (list 0 (- M column 1))))
                         ;; 2
                         (:right (3 ,day22/right (list row 0))
                          :down (5 ,day22/up (list (- M row 1)  column))
                          :left (6 ,day22/up (list (1- M) (- M row 1)))
                          :up (1 ,day22/down (list 0 (- M column 1))))
                         ;; 3
                         (:right (4 ,day22/right (list row 0))
                          :down (5 ,day22/right (list (- M column 1) 0))
                          :left (2 ,day22/left (list row (1- M)))
                          :up (1 ,day22/right (list column 0)))
                         ;; 4
                         (:right (6 ,day22/down (list 0 (- M row 1)))
                          :down (5 ,day22/down (list 0 column))
                          :left (3 ,day22/left (list row (1- M)))
                          :up (1 ,day22/up (list (1- M) column)))
                         ;; 5
                         (:right (6 ,day22/right (list row 0))
                          :down (2 ,day22/up (list (1- M) (- M column 1)))
                          :left (3 ,day22/up (list (1- M) (- M row 1)))
                          :up (4 ,day22/up (list (1- M) column)))
                         ;; 6
                         (:right (1 ,day22/left (list (- M row 1) (1- M)))
                          :down (2 ,day22/right (list (- M column 1) 0))
                          :left (5 ,day22/left (list row (1- M)))
                          :up (4 ,day22/left (list (- M column 1) (1- M))))))

(defvar day22/example-specifics (list :get-M-f #'day22/get-M-example
                                      :get-face-start-corner-f #'day22/get-face-start-corner-example
                                      :topology day22/example-topology))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem specific functions

(defun day22/get-M-problem (board)
  (/ (length board) 4))

(defun day22/get-face-start-corner-problem (M index)
  (elt (list
        nil
        (cons 0 M)
        (cons 0 (* 2 M))
        (cons M M) 
        (cons (* 2 M) 0) 
        (cons (* 2 M) M) 
        (cons (* 3 M) 0))
       index))

;; The last form should be fed the local coordinates for the starting face modulo M
(setq day22/problem-topology `(nil            ;we start from 1
                         ;; 1
                         (:right (2 ,day22/right (list row 0))
                          :down (3 ,day22/down (list 0 column))
                          :left (4 ,day22/right (list (- M row 1) 0))
                          :up (6  ,day22/right (list column 0)))
                         ;; 2
                         (:right (5 ,day22/left (list (- M row 1) (1- M)))
                          :down (3 ,day22/left (list column (1- M)))
                          :left (1 ,day22/left (list row (1- M)))
                          :up (6 ,day22/up (list (1- M) column)))
                         ;; 3
                         (:right (2 ,day22/up (list (1- M) row))
                          :down (5 ,day22/down (list 0 column))
                          :left (4 ,day22/down (list 0 row))
                          :up (1 ,day22/up (list (1- M) column)))
                         ;; 4
                         (:right (5 ,day22/right (list row 0))
                          :down (6 ,day22/down (list 0 column))
                          :left (1 ,day22/right (list (- M row 1) 0))
                          :up (3 ,day22/right (list column 0)))
                         ;; 5
                         (:right (2 ,day22/left (list (- M row 1) (1- M)))
                          :down (6 ,day22/left (list column (1- M)))
                          :left (4 ,day22/left (list row (1- M)))
                          :up (3 ,day22/up (list (1- M) column)))
                         ;; 6
                         (:right (5 ,day22/up (list (1- M) row))
                          :down (2 ,day22/down (list 0 column))
                          :left (1 ,day22/down (list 0 row))
                          :up (4 ,day22/up (list (1- M) column)))))

(setq day22/problem-specifics (list :get-M-f #'day22/get-M-problem
                                      :get-face-start-corner-f #'day22/get-face-start-corner-problem
                                      :topology day22/problem-topology))


(defun day22/direction-to-displacement (direction)
  (case direction
    (0 '(0 . 1))
    (1 '(1 . 0))
    (2 '(0 . -1))
    (3 '(-1 . 0))))

(defun day22/pos-to-local (specifics M face pos)
  (advent/assert face)
  (let ((start-corner (funcall (plist-get specifics :get-face-start-corner-f) M face)))
    (cons (- (car pos) (car start-corner))
          (- (cdr pos) (cdr start-corner)))))

(defun day22/local-to-pos (specifics M face local)
  (advent/assert face)
  (let ((start-corner (funcall (plist-get specifics :get-face-start-corner-f)M face)))
    (cons (+ (car local) (car start-corner))
          (+ (cdr local) (cdr start-corner)))))

(defun day22/candidate-coordinate (pos direction)
  (let ((displacement (day22/direction-to-displacement direction)))    
    (cons (+ (car pos) (car displacement))
          (+ (cdr pos) (cdr displacement)))))

;; TODO/FIXME this is so stupid…
(defun day22/find-wrap-direction (direction)
  (case direction
    (0 :right)
    (1 :down)
    (2 :left)
    (3 :up)))

(defun day22/compute-new-orientation (specifics M old-face old-wrapped-local overflow-direction)
  (seq-let (new-face new-direction coord-sexpr)
      (plist-get (elt (plist-get specifics :topology) old-face) overflow-direction)
    (let ((new-local-coordinates (eval coord-sexpr
                                       (list (cons 'M M)
                                             (cons 'row (car old-wrapped-local))
                                             (cons 'column (cdr old-wrapped-local))))))
      (list new-face new-direction (cons (car new-local-coordinates)
                                         (cadr new-local-coordinates))))))

(defun day22/get-face (specifics M pos)
  (let ((face (cond
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 1)) 1)
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 2)) 2)
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 3)) 3)
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 4)) 4)
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 5)) 5)
               ((day22/on-face M pos (funcall (plist-get specifics :get-face-start-corner-f) M 6)) 6)
               (t nil))))
    (advent/assert face)
    face))

(defun day22/next-pos-dir (specifics M pos-dir)
  (let* ((current-pos (car pos-dir))
        (current-dir (cadr pos-dir))
        (current-face (day22/get-face specifics M current-pos))
        (current-local (day22/pos-to-local specifics M current-face current-pos)))
    (let* ((next-local (day22/candidate-coordinate current-local current-dir))
           (wrapped-local (cons (mod (car next-local) M)
                                (mod (cdr next-local) M))))
      (if (equal next-local wrapped-local)
          ;; Same face, same-direction: just return the next coordinates converted to global
          (list (day22/local-to-pos specifics M current-face next-local) current-dir)
        ;; Different face somehow: find where I'm overflowing and use that to compute
        ;; the new face, direction and local coordinate
        (seq-let (new-face new-direction new-local-coordinates)
            (day22/compute-new-orientation specifics M current-face wrapped-local (day22/find-wrap-direction current-dir))
          (list (day22/local-to-pos specifics M new-face new-local-coordinates)
                new-direction))))))

;; TODO/FIXME this could be extracted, but whatever
(defun day22/new-3d-position-direction (specifics board position direction)
  "Returns the new position or nil in case of wall"
  (let* ((M (funcall (plist-get specifics :get-M-f) board))
         (new-pos-direction (day22/next-pos-dir specifics M (list position direction)))
         (tile (day22/get-tile board (car new-pos-direction))))
    (cond 
     ((= day22/empty tile)
      (error "Unexpected empty tile"))
     ((= day22/floor tile)
      new-pos-direction)
     ((= day22/wall tile)
      nil))))

(defun day22/translate-3d (specifics board position direction translation)
  (let ((steps 0)
        (stop))
    (while (and (not stop) (< steps translation )) 
      (if-let ((new-position-direction (day22/new-3d-position-direction specifics
                                                                        board
                                                                        position
                                                                        direction)))
          (progn
            (setq position (car new-position-direction))
            (setq direction (cadr new-position-direction)))        
        (setq stop t))
      (setq steps (1+ steps)))
    (list position direction)))

(defun day22/translate-3d-state (specifics state)
  (let ((board (day22-state-board state))
        (direction (day22-state-direction state))
        (moves (day22-state-moves state)))
    (let ((new-pos-dir (day22/translate-3d specifics
                                           board
                                           (day22-state-pos state)
                                           direction
                                           (caar moves))))      
      (make-day22-state :board board
                        :pos (car new-pos-dir)
                        :direction (cadr new-pos-dir)
                        :moves (cdr moves)))))

(defun day22/move-3d (specifics state)
  (let ((moves (day22-state-moves state)))
    (if (car (car moves))
        (day22/translate-3d-state specifics state)
      (day22/rotate-state state))))

(defun day22/consume-moves-3d (specifics state)
;;  (day22/debug-print-state state)
  (while (day22-state-moves state)
    (setq state (day22/move-3d specifics state))
;;    (day22/debug-print-state state)
    )
  state)

(defun day22/part-2 (line-blocks type)  
  (print "* Warning: this solution requires manual modification of the code")
  (day22/compute-password
   (day22/consume-moves-3d (if (eq type :example)
                               day22/example-specifics
                             day22/problem-specifics)
                           (day22/create-initial-state
                            (day22/read-problem line-blocks)))))

(provide 'day22)
