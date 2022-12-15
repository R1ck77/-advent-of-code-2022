(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 15 :example))
(defconst problem (advent/read-problem-lines 15 :problem))

(defstruct  day15-data "Definition for a sensor and associated beacon"
            sensor
            beacon
            range)

(defun day15/read-line-data (line)
  (-map #'string-to-number (rest
                            (s-match "Sensor at x=\\(.+\\), y=\\(.+\\): closest beacon is at x=\\(.+\\) y=\\(.+\\)"
                                     line))))

(defun day15/manhattan (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

(defun day15/read-beacon-line (line)
  (seq-let (sensor-x sensor-y beacon-x beacon-y) (day15/read-line-data line)
    (let ((sensor (cons sensor-x sensor-y))
          (beacon (cons beacon-x beacon-y)))
     (make-day15-data :sensor sensor
                      :beacon beacon
                      :range (day15/manhattan sensor beacon)))))

(defun day15/read-problem (lines)
  (-map #'day15/read-beacon-line lines))

(defun day15/sensor-distance (sensor-data x&y)
  (day15/manhattan (day15-data-sensor sensor-data) x&y))

(defun day15/sensor-interval (sensor-data y)
  (let* ((sensor (day15-data-sensor sensor-data))
         (range (day15-data-range sensor-data))
         (v-distance (abs (- (cdr sensor) y)))
         (a (+ (- (car sensor) range) v-distance))
         (b (- (+ (car sensor) range) v-distance)))
    (and (<= a b) (cons a b))))

(defun day15/all-intervals (sensors-data y)
  (--filter it (--map (day15/sensor-interval it y) sensors-data)))

;;; Probably not needed
(defun day15/sort-intervals (a b)
  (or
   (< (car a) (car b))
   (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

(defun day15/in-between? (x range)
  (and (>= x (car range))
       (<= x (cdr range))))

(defun day15/intersects (a b)
  (or (day15/in-between? (car a) b)
      (day15/in-between? (cdr a) b)))

(defun day15/join-all-intervals (intervals)
  (let ((table (make-hash-table)))
    ;; TODO Let's do it the dumb way and see how it goes
    (-each (--map (number-sequence (car it) (cdr it)) intervals)
      (lambda (interval)
        (--each interval
          (advent/put table it t))))
    table))

(defun day15/remove-beacons! (sensors-data points-set y)
  (--each sensors-data
    (let ((beacon (day15-data-beacon it)))
      (if (= (cdr beacon) y)
          (remhash (car beacon) points-set))))
  points-set)

(defun day15/points-in-set (points-set)
  (hash-table-count points-set))

(defun day15/part-1 (lines y)
  (let ((sensors-data (day15/read-problem lines)))
    (day15/points-in-set
     (day15/remove-beacons! sensors-data
                            (day15/join-all-intervals (day15/all-intervals sensors-data
                                                                           y))
                            y))))

(defun day15/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day15)
