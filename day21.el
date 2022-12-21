(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 21 :example))
(defconst problem (advent/read-problem-lines 21 :problem))

(defconst day21/root-monkey :root)

(defun day21/strip-colon (token)
  (s-replace ":" "" token))

(defun day21/read-yell-monkey (table tokens)
  (seq-let (raw-name raw-number) tokens
    (advent/put table
                (day21/strip-colon raw-name)
                (string-to-number raw-number))))

;; TODO/FIXME why?
(setq THIS_DOESNT_WORK (list (cons "+" #'+)
                         (cons "-" #'-)
                         (cons "/" #'/)
                         (cons "*"  #'*)))

(defun day21/read-compute-monkey (table tokens)
  (seq-let (raw-name op1 raw-operator op2) tokens
    (advent/put table
                (day21/strip-colon raw-name)
                (list (cdr (assoc raw-operator (list (cons "+" #'+)
                                                     (cons "-" #'-)
                                                     (cons "/" #'/)
                                                     (cons "*"  #'*))))
                      op1
                      op2))))

(defun day21/read-monkey (table line)
  (let ((tokens (split-string line)))
    (if (= (length tokens) 2)
        (day21/read-yell-monkey table tokens)
      (day21/read-compute-monkey table tokens))))

(defun day21/read-problem (lines)
  (let ((table (advent/table)))
    (--map (day21/read-monkey table it) lines)
    table))

(defun day21/solve-expression! (monkeys value expression)
  (seq-let (operation op1 op2) expression
    (let ((result (funcall operation
                           (day21/compute-value! monkeys op1)
                           (day21/compute-value! monkeys op2))))
      (advent/put monkeys value result)
      result)))

(defun day21/compute-value! (monkeys value)
  (let ((raw-value (advent/get monkeys value)))
    (if (numberp raw-value)
        raw-value
      (day21/solve-expression! monkeys value raw-value))))

(defun day21/get-root-value! (monkeys)
  (day21/compute-value! monkeys "root"))

(defun day21/part-1 (lines)
  (setq max-specpdl-size 10000) ;TODO/FIXME why do I need this only for the console?
  (day21/get-root-value! (day21/read-problem lines)))

(defun day21/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day21)
