(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 25 :example))
(defconst problem (advent/read-problem-lines 25 :problem))

(defun day25/snafu-to-decimal (snafu)
  (car
   (-reduce-from (lambda (acc&exp digit)
                   (let ((value (case digit
                                  (?2 2)
                                  (?1 1)
                                  (?0 0)
                                  (?- -1)
                                  (?= -2)
                                  (t (error "Unexpected digit"))))
                         (mult (expt 5 (cdr acc&exp))))
                     (cons (+ (car acc&exp) (* value mult)) (1+ (cdr acc&exp)))))
                 '(0 . 0 )
                 (reverse (append snafu nil)))))

(defun day25/decimal-to-snafu (decimal)
  "12")

(defun day25/part-1 (lines)
  (error "Not yet implemented"))

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)
