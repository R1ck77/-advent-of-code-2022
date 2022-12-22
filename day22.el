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
    (list (list (string-to-number (substring line 0 movement-size)) 0)
          (substring line movement-size))))

(defun day22/take-next-turn (line)
  (let ((s-movement (substring line 0 1))
        (remaining (substring line 1)))
    (list (list 0 (cond
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
                :size (cons (length board) (length (elt board 0)))
                :moves (day22/read-moves (cadr blocks)))))

(defconst e (day22/read-problem example))
(defconst p (day22/read-problem problem))

(defun day22/row-as-list (board row)
  (append (elt (plist-get board :board) row) nil))

(defun day22/get-start-coordinates (board)
  (cons 0
        (car (--first (= (cdr it) day22/floor)
                  (--map-indexed (cons it-index it)
                                 (day22/row-as-list board 0))))))

(defun day22/part-1 (lines)
  (error "Not yet implemented"))

(defun day22/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day22)
