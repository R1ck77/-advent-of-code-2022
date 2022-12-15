(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 15 :example))
(defconst problem (advent/read-problem-lines 15 :problem))

(defstruct  day15-data "Definition for a sensor and associated beacon"
            sensor
            beacon)

(defun day15/read-line-data (line)
  (-map #'string-to-number (rest
                            (s-match "Sensor at x=\\(.+\\), y=\\(.+\\): closest beacon is at x=\\(.+\\) y=\\(.+\\)"
                                     line))))

(defun day15/read-beacon-line (line)
  (seq-let (sensor-x sensor-y beacon-x beacon-y) (day15/read-line-data line)
    (make-day15-data :sensor (cons sensor-x sensor-y)
                     :beacon (cons beacon-x beacon-y))))

(defun day15/read-problem (lines)
  (-map #'day15/read-beacon-line lines))

(defun day15/part-1 (lines)
  (error "Not yet implemented"))

(defun day15/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day15)
