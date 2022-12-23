(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 23 :example))
(defconst problem (advent/read-problem-lines 23 :problem))

(defstruct day23-state "Positions of the elves and their internal states"
           elves ; position -> index (row,column)
           rules ); index -> rule

(defun day23/read-problem (lines)
  (let ((grid (advent/lines-to-grid lines (lambda (x) (if (string= x ".") 0 1))))
        (elves (advent/table))
        ;; Not needed at the moment, but I foresee a "next part, each elf choses it's own direction…
        (rules (make-hash-table)))
    (advent/-each-grid grid
      (when (= it-value 1)
        (advent/put elves it-coord (advent/table-size rules))
        ;; All elves propose the same rule for now, so this is unused
        (advent/put rules (advent/table-size rules) 0) 
        ))
    (make-day23-state :elves elves
                      :rules rules)))

(setq e (day23/read-problem example))
(setq p (day23/read-problem problem))

(defmacro day23/update (f value new-value)
  (let ((v (make-symbol "old"))
        (n (make-symbol "new")))
    `(let ((,v ,value)
           (,n ,new-value))
      (setq ,value (if (or (not ,v) (funcall ,f ,n ,v))
                       ,n
                     ,v)))))

(defun day23/get-corners (state)
  "Return the minimum and maximum coordinate for the elves"
  (let ((min-row)
        (min-column)
        (max-row)
        (max-column))
    (advent/-each-hash (day23-state-elves state)
      (day23/update #'< min-row (car it-key))
      (day23/update #'< min-column (cdr it-key))
      (day23/update #'> max-row (car it-key))
      (day23/update #'> max-column (cdr it-key)))
    (list (cons min-row min-column)
          (cons max-row max-column))))

(defun day23/to-buffer (state buffer)
  (let ((elves (day23-state-elves state))
        (pos))
    (seq-let (min max) (day23/get-corners state)
      (let ((rows (- (car max) (car min)))
            (columns (- (cdr max) (cdr min))))
        (with-current-buffer buffer
          (erase-buffer)
          (-dotimes (1+ rows)
            (lambda (row)
              (-dotimes (1+ columns)
                (lambda (column)
                  (insert (if (advent/get elves (cons (+ (car min) row)
                                                      (+ (cdr min) column)))
                              "#"
                            "."))))
              (insert "\n"))))))))

(defun day23/debug-print (state)
  (let ((buffer (get-buffer-create "*day23*")))
    (day23/to-buffer state buffer)
    (display-buffer buffer)
    (sit-for 0.1)))

(defun day23/to-string (state)
  (with-temp-buffer
    (day23/to-buffer state (current-buffer))
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun day23/part-1 (lines)
  (error "Not yet implemented"))

(defun day23/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day23)
