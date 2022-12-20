(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 19 :example))
(defconst problem (advent/read-problem-lines 19 :problem))

(defconst day19/total-time 24)

(defconst day19/ore-index 0)
(defconst day19/clay-index 1)
(defconst day19/obs-index 2)
(defconst day19/geo-index 3)

(defstruct day19-bprint "Blueprint description"
  id costs)

(defun day19/read-geo-r (definition)
  (seq-let (_ s-ore s-obs)
      (s-match "Each geode robot costs \\(.+\\) ore and \\(.+\\) obsidian"
               definition)
    (list (string-to-number s-ore)
          0
          (string-to-number s-obs)
          0)))

(defun day19/read-obs-r (definition)
  (seq-let (_ s-ore s-clay)
      (s-match "Each obsidian robot costs \\(.+\\) ore and \\(.+\\) clay"
               definition)
    (list (string-to-number s-ore)
          (string-to-number s-clay)
          0
          0)))

(defun day19/read-clay-r (definition)
  (let ((ore (string-to-number
              (cadr
               (s-match "Each clay robot costs \\(.+\\) ore"
                        definition)))))
    (list ore 0 0 0)))

(defun day19/read-ore-r (definition)
  (let ((ore (string-to-number
              (cadr
               (s-match "Each ore robot costs \\(.+\\) ore"
                        definition)))))
    (list ore 0 0 0)))

(defun day19/read-id (definition)
  (string-to-number
   (cadr
    (s-match "Blueprint \\(.+\\)"
             definition))))

(defun day19/read-blueprint (line)
  (seq-let (raw-bprint-id
            raw-ore-r
            raw-clay-r
            raw-obs-r
            raw-geo-r)
      (split-string line "[.:] ?" t)
    (make-day19-bprint :id (day19/read-id raw-bprint-id)
                       :costs (list (day19/read-ore-r raw-ore-r)
                                    (day19/read-clay-r raw-clay-r)
                                    (day19/read-obs-r raw-obs-r)
                                    (day19/read-geo-r raw-geo-r)))))

(defun day19/op (f v1 v2)
  (--map (funcall f (car it) (cdr it)) (-zip v1 v2)))

(defun day19/vop (f v m)
  (--map (funcall f it m) v))

(defstruct day19-state "Simulation state"
           resources
           robots
           time)

(defun day19/create-starting-state ()
  (make-day19-state :time 0
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)))

(defun day19/is-buildable? (resources cost)
  (> (apply #'min (day19/op (lambda (a b)
                              (if (zerop b)
                                  most-positive-fixnum
                                (/ a b)))
                            resources cost))
     0))

(defun day19/sub-mul (v1 v2 m)
  "res = v1 - m * v2"
  (day19/op (lambda (a b) (- a (* m b))) v1 v2))

(defun day19/build-cost (costs build)
  (--reduce (day19/op #'+ acc it)
            (list (day19/vop #'* (elt costs 0) (elt build 0))
                  (day19/vop #'* (elt costs 1) (elt build 1))
                  (day19/vop #'* (elt costs 2) (elt build 2))
                  (day19/vop #'* (elt costs 3) (elt build 3)))))

(defun day19/compute-new-resources (current gain costs)
  (day19/op #'- (day19/op #'+ current gain) costs))

(defun day19/get-buildable-robots (bprint state)
  "Returns a list of robots that can be built"
  (let ((resources (day19-state-resources state))
        (costs (day19-bprint-costs bprint)))
    (--map (day19/is-buildable? resources (elt costs it)) (number-sequence 0 3))))

(defun day19/evolve-resources (now current robots)
  "Evolve the resources with the status quo until day 24"
  (let ((evolution (list (list now current))))
    (--dotimes (- day19/total-time now)
      (setq now (1+ now))
      (setq current (day19/compute-new-resources current robots '(0 0 0 0)))
      (setq evolution (cons (list now current) evolution)))
    (nreverse evolution)))

(defun day19/evolve-state-resources (state)
  "Evolve the resources for the state until day 24"
  (day19/evolve-resources (day19-state-time state)
                          (day19-state-resources state)
                          (day19-state-robots state)))


(defun day19/eta-for-robot (times-resources cost)
  "ETA for having the robot built (includes building time)"
  (car (cdr (--drop-while (let ((resources (cadr it)))
                        (< (apply #'min (day19/op #'- resources cost)) 0))
                      times-resources))))

(defun day19/jump-state-to-construction (bprint state robot-index)
  (let ((evolved-resources (day19/evolve-state-resources state))
        (robot-cost (elt (day19-bprint-costs bprint) robot-index)))
    (if-let ((expected-deadline (day19/eta-for-robot evolved-resources robot-cost)))
        (let ((resources-after-construction (day19/compute-new-resources (cadr expected-deadline)
                                                                         '(0 0 0 0) ; already accounted
                                                                         robot-cost)))
          (make-day19-state :resources resources-after-construction
                            :robots (-update-at robot-index #'1+ (day19-state-robots state))
                            :time (car expected-deadline))))))


(defun day19/compute-blueprint-quality (bprint)
  (setq *best-result* 0)
  (day19/compute-quality (day19/create-starting-state bprint)))

(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
