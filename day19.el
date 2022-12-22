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

(defconst day19/indices (list day19/ore-index
                              day19/clay-index
                              day19/obs-index
                              day19/geo-index))

(defstruct day19-bprint "Blueprint description"
  id costs ideal-times)

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
                                    (day19/read-geo-r raw-geo-r))
                       :ideal-times nil)))

(defun day19/op (f v1 v2)
  (--map (funcall f (car it) (cdr it)) (-zip v1 v2)))

(defun day19/minimum-turns-before-construction (cost)
  "Time required to have resources >= res starting without robots.

It assumes - ideally - that all the resources to build infinite robots are present"
  (ceiling (/ (1- (sqrt (+ 1 (* 8 cost)))) 2)))

(defun day19/turns-for-enough-clay-for-obs (bprint)
  "Irrealistic number of turns required to have enough clay to build an obsidian robot

Assuming no initial clay robot is present"
  ;; TODO/FIXME not sure about a missing +1
  (day19/minimum-turns-before-construction (elt (elt (day19-bprint-costs bprint)
                                                     day19/obs-index)
                                                day19/clay-index)))

(defun day19/turns-for-enough-obs-for-geo (bprint)
  "Irrealistic number of turns required to have enough obsidian to build a geo robot

Assuming no initial obsidian robot is present"
  ;; TODO/FIXME not sure about a missing +1
  (day19/minimum-turns-before-construction (elt (elt (day19-bprint-costs bprint)
                                                     day19/geo-index)
                                                day19/obs-index)))

(defun day19/turns-for-enough-clay-for-geo (bprint)
  "Irrealistic number of turns require to have enough clay to build a geo robot

Assums no clay robot is present and that perfect production of clay robots and obsidian occurs"
  ;; TODO/FIXME not sure about a missing +1
  (+ (day19/turns-for-enough-clay-for-obs bprint)
     (day19/turns-for-enough-obs-for-geo bprint)))

(defun day19/add-ideal-times-to-blueprint! (bprint)
  (setf (day19-bprint-ideal-times bprint) (cons (day19/turns-for-enough-clay-for-geo bprint)
                                                (day19/turns-for-enough-obs-for-geo bprint)))
  bprint)



(defstruct day19-state "Simulation state"
           resources
           robots
           time)

(defun day19/create-starting-state ()
  (make-day19-state :time 0
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)))

(defun day19/compute-new-resources (current gain costs)
  (day19/op #'- (day19/op #'+ current gain) costs))

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

(defun day19/evolve-state-resources-to-end (state)
  (let ((time-resources-evolution (car (last (day19/evolve-state-resources state)))))
    (advent/assert (= (car time-resources-evolution) day19/total-time) "Invalid end time?")
    (make-day19-state :resources (cadr time-resources-evolution)
                      :robots (day19-state-robots state)
                      :time (car time-resources-evolution))))


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

(defun day19/evaluate-sequence (bprint state moves)
  (-reduce-from (lambda (state move)
             (if state
                 (day19/jump-state-to-construction bprint state move)))
                state moves))

(defun day19/state-score (state)
  (when state
    (elt (day19-state-resources (day19/evolve-state-resources-to-end state)) day19/geo-index)))

(defun day19/compute-maximal (bprint state)
  (or 1000
   (let ((costs (day19-bprint-costs bprint))
         (robots (day19-state-robots state))
         (end-resources (day19-state-resources (day19/evolve-state-resources-to-end state))))
     ;; easy version: don't count how many resources I have, only how many I built
     ;; also don't check anything but geode robots
     )))

(defun day19/compute-production-delay (bprint state)
    (let ((robots (day19-state-robots state))
          (now (day19-state-time state)))
      (cond
       ((zerop (elt robots day19/clay-index))
        (car (day19-bprint-ideal-times bprint)))
       ((zerop (elt robots day19/obs-index))
        (cdr (day19-bprint-ideal-times bprint)))
       ((zerop (elt robots day19/geo-index))
        0) ;TODO/FIXME underestimated. Probably it's 1
       (t 0))))

(defun day19/maximum-ideal-result (state production-delay)
  (let ((actual-production (elt (day19-state-resources (day19/evolve-state-resources-to-end state)) day19/geo-index))
        (available-time (- day19/total-time (day19-state-time state) production-delay)))
    (+ actual-production (/ (* available-time (1- available-time)) 2))))

(defun day19/ideal-result (bprint state)
  (let ((delay (day19/compute-production-delay bprint state))
        (now (day19-state-time state)))
    (let ((result (if (> (+ delay now) day19/total-time)
               (elt (day19-state-resources (day19/evolve-state-resources-to-end state)) day19/geo-index)
               (day19/maximum-ideal-result state delay))
                  ))
      (if (zerop result) -1 0))))

(defun day19/evaluate-sequence-with-check (bprint state path last-state)
  ;; TODO/FIXME is there a teoretical max? Yep
  (if (and (< (--count (= it day19/ore-index) path) 8)
           (> (day19/compute-maximal bprint last-state) *best-result*))    
      (day19/jump-state-to-construction bprint last-state (car path))
    nil))

(defun day19/next-combinations (bprint state path last-state)
  (--map (list (day19/state-score (cadr it))
               (cadr it)
               (car it))
         (--filter (cadr it)
                   (--map (list it
                                (day19/evaluate-sequence-with-check bprint
                                                                    state
                                                                    it
                                                                    last-state))
                          (--map (cons it path) (reverse day19/indices))))))

;;; TODO/FIXME I definitely have to remove those horrors :p
(defvar *best-result* )

(defun day19/search-best (bprint state path last-state)
  (comment
    (print (reverse path))
    (sit-for 0.01))
  (advent/assert state "nil state?")
  (let ((score-state-path-list (day19/next-combinations bprint state path (or last-state state))))
    (if score-state-path-list
        (apply #'max (or (--map (day19/search-best bprint state (elt it 2) (elt it 1))
                                score-state-path-list)
                         '(-1)))
      (let ((result (day19/state-score last-state)))
        (when (> result *best-result*)
          (print (format "%s -> %s" (reverse path) result))
          (setq *best-result* result)
          (sit-for 0.01))
        result))))

(defun day19/compute-quality (bprint)
  (setq *best-result* 0)
  (day19/search-best bprint
                     (day19/create-starting-state)
                     nil
                     nil))

(defun day19/compute-blueprint-quality (bprint)
  (day19/state-score (car (nreverse (day19/evolve-blueprint bprint)))))

(defun day19/read-problem (lines)
  (-map #'day19/add-ideal-times-to-blueprint! (-map #'day19/read-blueprint lines)))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
