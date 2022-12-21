(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 21 :example))
(defconst problem (advent/read-problem-lines 21 :problem))

(defconst day21/root-monkey "root")
(defconst day21/me "humn")

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

(setq e (day21/read-problem example))
(setq p (day21/read-problem problem))

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
  (day21/compute-value! monkeys day21/root-monkey))

(defun day21/part-1 (lines)
  (setq max-specpdl-size 10000) ;TODO/FIXME why do I need this only for the console?
  (day21/get-root-value! (day21/read-problem lines)))

(defun day21/get-root-subordinates (monkeys)
  (seq-let (_ sub1 sub2) (advent/get monkeys day21/root-monkey)
    (list sub1 sub2)))

;; too simple to be true
(defun day21/dumb-compute-subordinates (monkeys input)
  (let ((copy (copy-hash-table monkeys)))
    (advent/put copy day21/me input)
    (--map (day21/compute-value! copy it)
           (day21/get-root-subordinates monkeys))))

(defun day21/try-resolve-number! (monkeys name-or-number)
  (unless (numberp name-or-number)
    (let ((result (advent/get monkeys name-or-number)))
      (and (numberp result) result))))

(defun day21/simplify (monkeys)
  (let ((copy (copy-hash-table monkeys))
        (optimizing -1))
    (remhash day21/me copy)
    (while (not (zerop optimizing))
      (setq optimizing 0)
      (advent/-each-hash copy
        (unless (numberp it-value)
          (seq-let (op v1 v2) it-value
            (if-let ((new-v1 (day21/try-resolve-number! copy v1)))
                (progn
                  (setq optimizing (1+ optimizing))
                  (setq v1 new-v1)))
            (if-let ((new-v2 (day21/try-resolve-number! copy v2)))
                (progn
                  (setq optimizing (1+ optimizing))
                  (setq v2 new-v2)))
            (if (and (numberp v1) (numberp v2))
                (advent/put copy it-key (funcall op v1 v2))
              (advent/put copy it-key (list op v1 v2)))))))
    copy))

(defun day21/resolve-simbolic-expression (monkeys value)
  (if (numberp value)
      value
    (let ((content (advent/get monkeys value :me)))
      (cond
       ((eq content :me) day21/me)
       ((numberp content) content)
       (t (list (elt content 0)
                (day21/resolve-simbolic-expression monkeys (elt content 1))
                (day21/resolve-simbolic-expression monkeys (elt content 2))))))))

(defun day21/get-equation (monkeys)
    (let ((simplified (day21/simplify monkeys)))
      (seq-let (expr1 expr2) (--map (day21/resolve-simbolic-expression simplified it)
                                    (day21/get-root-subordinates simplified))
        (if (numberp expr1)
            (list expr2 expr1)
          (list expr1 expr2)))))

(defun day21/simplify-expr (expr number)
  (seq-let (op v1 v2) expr
    (cond
     ;; x = (/ expr 4) -> expr = 4 * x
     ((eq op #'/) (list v1 (* number v2)))
     ((eq op #'-) (if (numberp v1)
                      ;; x = (- v1 expr) -> expr = - x + v1
                      (list v2 (- v1 number))
                    ;; x = (- expr v2) -> expr = x - v2
                    (list v1 (+ number v2))))
     ((eq op #'*) (if (numberp v1)
                      ;; x = (* v1 expr) -> expr = x / v1
                      (list v2 (/ number v1))
                    ;; x = (* expr v2) -> expr = x / v2
                    (list v1 (/ number v2))))
     ((eq op #'+) (if (numberp v1)
                      ;; x = (+ v1 expr) -> epr = x - v1
                      (list v2 (- number v1))
                    ;; x = (+ expr v2) -> epr = x - v2
                    (list v1 (- number v2))))
     (t (error (format "Unexpected condition '%s'" expr))))))

(defun day21/solve-for-human (monkeys)
  (seq-let (expr number) (day21/get-equation monkeys)
    (while (not (equal expr day21/me))
      ;; TODO/FIXME just use the list
      (seq-let (new-expr new-number) (day21/simplify-expr expr number)
        (setq expr new-expr)
        (setq number new-number)))
    number))

(defun day21/debug-print-monkeys (monkeys)
  (advent/-each-hash monkeys
    (print (format "%s: %s" it-key it-value))))

(defun day21/part-2 (lines)
  (day21/solve-for-human (day21/read-problem lines)))

(provide 'day21)
