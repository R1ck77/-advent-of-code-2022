(require 'dash)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 10 :example))
(defvar problem (advent/read-problem-lines 10 :problem))
(defvar tiny (list "noop" "addx 3" "addx -5"))

(defconst day10/interesting-checks '(20 60 100 140 180 220))

(defun day10/read-line (line)
  (let ((tokens (split-string line)))
    (case (length tokens)
      (1 :noop)
      (2 (list :addx (string-to-number (cadr tokens))))
      (t (error (format "Invalid instruction '%s'?" line))))))

(defun day10/read-instructions (lines)
  (-map #'day10/read-line lines))

(defun day10/convert-addx-to-plain-add (instructions)
  (--mapcat (if (eq :noop it)
                '(0)
              `(0 ,(cadr it)))
            instructions))

(defun day10/sum-elements (acc el)
  (let ((sequence (plist-get acc :seq))
        (register (plist-get acc :reg)))
    (list :reg (+ (or el 0) register)
          :seq (cons register sequence))))

(defun day10/compute-evolution (instructions)
  (reverse (plist-get (-reduce-from #'day10/sum-elements
                            (list :reg 1 :seq nil)
                            instructions)
              :seq)))

(defun day10/get-interesting-values (sequence)
  (--map (list it (elt sequence (1- it))) day10/interesting-checks))

(defun day10/compute-strengths-sum (strengths)
  (apply #'+ (--map (apply #'* it) strengths)))

(defun day10/part-1 (lines)
  (day10/compute-strengths-sum
   (day10/get-interesting-values
    (day10/compute-evolution
     (day10/convert-addx-to-plain-add
      (day10/read-instructions lines))))))

(defun day10/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day10)
