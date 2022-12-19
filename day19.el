(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 19 :example))
(defconst problem (advent/read-problem-lines 19 :problem))

(defconst day19/total-time 24)

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

(defstruct day19-state "Simulation state"
  resources
  robots
  bprint
  time)

(defun day19/create-starting-state (bprint)
  (make-day19-state :time 0
                    :bprint bprint
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)))

(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
