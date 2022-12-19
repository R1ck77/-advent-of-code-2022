(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 19 :example))
(defconst problem (advent/read-problem-lines 19 :problem))

(defconst day19/total-time 24)

(defstruct day19-bprint "Blueprint description"
           id
           ore-r
           clay-r
           obs-r
           geo-r)

(defun day19/read-geo-r (definition)
  (seq-let (_ s-ore s-obs)
      (s-match "Each geode robot costs \\(.+\\) ore and \\(.+\\) obsidian"
               definition)
    (list :ore (string-to-number s-ore)
          :obs (string-to-number s-obs))))

(defun day19/read-obs-r (definition)
  (seq-let (_ s-ore s-clay)
      (s-match "Each obsidian robot costs \\(.+\\) ore and \\(.+\\) clay"
               definition)
    (list :ore (string-to-number s-ore)
          :clay (string-to-number s-clay))))

(defun day19/read-clay-r (definition)
  (list :ore (string-to-number
              (cadr
               (s-match "Each clay robot costs \\(.+\\) ore"
                        definition)))))

(defun day19/read-ore-r (definition)
  (list :ore (string-to-number
              (cadr
               (s-match "Each ore robot costs \\(.+\\) ore"
                        definition)))))

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
                       :ore-r (day19/read-ore-r raw-ore-r)
                       :clay-r (day19/read-clay-r raw-clay-r)
                       :obs-r (day19/read-obs-r raw-obs-r)
                       :geo-r (day19/read-geo-r raw-geo-r))))

(defun day19/ore-r-cost (bprint)
  (plist-get (day19-bprint-ore-r bprint) :ore))

(defun day19/clay-r-cost (bprint)
  (plist-get (day19-bprint-clay-r bprint) :ore))

(defun day19/obs-r-cost (bprint)
  (let ((obs-r (day19-bprint-obs-r bprint)))
    (list (plist-get obs-r :ore)
          (plist-get obs-r :clay))))

(defun day19/geo-r-cost (bprint)
  (let ((obs-r (day19-bprint-obs-r bprint)))
    (list (plist-get obs-r :ore)
          (plist-get obs-r :obs))))

(defstruct day19-state "Simulation state"
           ore clay obs geo
           ore-r clay-r obs-r geo-r
           bprint
           time)

(defun day19/create-starting-state (bprint)
  (make-day19-state :time 0
                    :bprint bprint
                    :ore 0 :clay 0 :obs 0 :geo 0
                    :ore-r 1 :clay-r 0 :obs-r 0 :geo-r 0))

(defun day19/compute-next-result (state)
  (advent/assert (< (day19-state-time state)
                    day19/total-time)
                 "Unexpected condition (time exceeded)")
  (cond )
  )


(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
