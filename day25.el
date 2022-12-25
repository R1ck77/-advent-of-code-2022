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

(defun day25/get-largest-digit (value)
  (let ((base 0))
    (while (< (expt 5 base) value)
      (setq base (1+ base)))
    base))

(defun day25/best-difference (value mult)
  (car (--sort (< (abs (car it))
              (abs (car other)))
           (--map (list (- value (* (car it) mult)) it)
                  '((-2 . "=")
                    (-1 . "-")
                    (0 . "0")
                    (1 . "1")
                    (2 . "2"))))))

(defun day25/factorize (rem-digits base)
  (seq-let (remainder _&digit)
      (day25/best-difference (car rem-digits) (expt 5 base))
    (list remainder (cons (cdr _&digit) (cadr rem-digits)))))

(defun day25/decimal-to-snafu-digits (decimal)
  (cadr
   (-reduce-from #'day25/factorize
                 (list decimal nil)
                 (reverse
                  (number-sequence 0
                                   (day25/get-largest-digit decimal))))))

(defun day25/decimal-to-snafu (decimal)
  (apply #'concat (reverse (day25/decimal-to-snafu-digits decimal))))

(defun day25/part-1 (lines)
  (apply #'+ (-map #'day25/snafu-to-decimal lines))
)

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)
