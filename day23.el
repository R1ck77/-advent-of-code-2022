(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day23/rules (list
                       (list :dest '(-1 . 0) :checks '((-1 . 0) (-1 . 1) (-1 . -1)))
                       (list :dest '(1 . 0) :checks '((1 . 0) (1 . 1) (1 . -1)))
                       (list :dest '(0 . -1) :checks '((0 . -1) (-1 . -1) (1 . -1)))
                       (list :dest '(0 . 1) :checks '((0 . 1) (-1 . 1) (1 . 1)))))

(defconst day23/all-neighbors '((-1 . -1) (-1 . 0) (-1 .  1)
                                ( 0 . -1)          ( 0 .  1)
                                ( 1 . -1) ( 1 . 0) ( 1 .  1)))

(defstruct day23-state "Positions of the elves and their internal states"
           elves ; position -> index (row,column)
           current-rule ); index -> rule

(defun day23/read-problem (lines)
  (let ((grid (advent/lines-to-grid lines (lambda (x) (if (string= x ".") 0 1))))
        (elves (advent/table)))
    (advent/-each-grid grid
      (when (= it-value 1)
        (advent/put elves it-coord t)))
    (make-day23-state :elves elves
                      :current-rule 0)))

(defmacro day23/update (f value new-value)
  (let ((v (make-symbol "old"))
        (n (make-symbol "new")))
    `(let ((,v ,value)
           (,n ,new-value))
       (setq ,value (if (or (not ,v) (funcall ,f ,n ,v))
                        ,n
                      ,v)))))

(defun day23/get-corners (state)
  "Return the rectangle containing all elves [min, max["
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
          (cons (1+ max-row) (1+ max-column)))))

(defun day23/to-buffer (state buffer)
  (let ((elves (day23-state-elves state))
        (pos))
    (seq-let (min max) (day23/get-corners state)
      (let ((rows (- (car max) (car min)))
            (columns (- (cdr max) (cdr min))))
        (with-current-buffer buffer
          (erase-buffer)
          (-dotimes rows
            (lambda (row)
              (-dotimes columns
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

(defun day23/sum-cons (a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(defun day23/all-free? (elves pos displacements)
  (not
   (--first (advent/get elves it)
            (--map (day23/sum-cons pos it) displacements))))

(defun day23/alone-elf? (state pos)
  "Returns t if there are no elves in the adjacent positions"
  (day23/all-free? (day23-state-elves state) pos day23/all-neighbors))

(defun day23/elf-proposal (state pos rules)
  "Return the proposed movement for the elf at pos.

The rules are a circular list with the first valid rule at car"
  (advent/assert (advent/get (day23-state-elves state) pos) "No elf at pos!")
  (let ((elves (day23-state-elves state)))
    (unless (day23/alone-elf? state pos)
      (if-let ((matching-rule (--first (day23/all-free? elves
                                                        pos
                                                        (plist-get it :checks))
                                       (-take 4 rules))))
          (day23/sum-cons (plist-get  matching-rule :dest) pos)))))

(defun day23/plan-elves-moves (state rules)
  "Returns a dictionary of moves for the elves.

Rules is a cyclic list starting with the current rule"
  (let ((moves-planned (advent/table)))
    ;; Find all proposals and count them
    (advent/-each-hash (day23-state-elves state)
      (if-let ((elf-proposal (day23/elf-proposal state it-key rules)))
          (advent/update moves-planned elf-proposal
                         (lambda (key old-value)
                           (if old-value
                               (cons it-key old-value)
                             (list it-key))))))
    (let ((moves))
      (advent/map-hash moves-planned
                       (lambda (move elves)
                         (if (= (length elves) 1)
                             (setq moves (cons (list (car elves) move) moves)))))
      moves)))

(defun day23/get-rules (state)
  "Get the cyclic list of the current rules"
  (nthcdr (day23-state-current-rule state) (-cycle day23/rules)))

(defun day23/update-rules (state)
  "Update the counter for every elf"
  (make-day23-state :current-rule (1+ (day23-state-current-rule state))
                    :elves (copy-hash-table (day23-state-elves state))))

(defun day23/move-elves (state moves)
  (let ((new-elves (copy-hash-table (day23-state-elves state))))
    (--each moves
      (seq-let (src dest) it
        (let ((rule-index (advent/get new-elves src)))
          (remhash src new-elves)
          (advent/put new-elves dest rule-index))))
    (make-day23-state :elves new-elves
                      :current-rule (1+ (day23-state-current-rule state)))))

(defun day23/step (state)
  (if-let ((moves (day23/plan-elves-moves state (day23/get-rules state))))
      (list (length moves) (day23/move-elves state moves))
    (list 0 (day23/update-rules state))))

(defun day23/evolve (state steps)
  (let ((step 0)
        (elf-moving -1))
    (while (and (< step steps)
                (not (zerop elf-moving)))
      (seq-let (new-moves new-state) (day23/step state)
        (setq state new-state)
        (setq elf-moving new-moves)
        (setq step (1+ step))))
    (list step state)))

(defun day23/rectangle-area (state)
  (seq-let (min max) (day23/get-corners state)
    (* (- (car max) (car min))
       (- (cdr max) (cdr min)))))

(defun day23/compute-empty (state)
  (let ((n (advent/table-size (day23-state-elves state))))
    (- (day23/rectangle-area state) n)))

(defun day23/part-1 (lines)
  (day23/compute-empty
   (cadr
    (day23/evolve (day23/read-problem lines)
                  10))))

(defun day23/part-2 (lines)
    (car
     (day23/evolve (day23/read-problem lines)
                   most-positive-fixnum)))

(provide 'day23)
