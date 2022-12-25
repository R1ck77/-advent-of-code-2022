(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 19 :example))
(defconst problem (advent/read-problem-lines 19 :problem))

(defconst day19/part-1-total-time 24)
(defconst day19/part-2-total-time 32)

(defconst day19/ore-index 0)
(defconst day19/clay-index 1)
(defconst day19/obs-index 2)
(defconst day19/geo-index 3)

(defconst day19/indices (list day19/ore-index
                              day19/clay-index
                              day19/obs-index
                              day19/geo-index))

(defstruct day19-bprint "Blueprint description"
  id costs )

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
           time)

(defun day19/create-starting-state ()
  (make-day19-state :time 0
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)))

(defun day19/compute-new-resources (current gain costs)
  (day19/op #'- (day19/op #'+ current gain) costs))

(defun day19/evolve-resources (now current robots total-time)
  "Evolve the resources with the status quo until total-time"
  (let ((evolution (list (list now current))))
    (--dotimes (- total-time now)
      (setq now (1+ now))
      (setq current (day19/compute-new-resources current robots '(0 0 0 0)))
      (setq evolution (cons (list now current) evolution)))
    (nreverse evolution)))

(defun day19/evolve-state-resources (state total-time)
  "Evolve the resources for the state until total-time"
  (day19/evolve-resources (day19-state-time state)
                          (day19-state-resources state)
                          (day19-state-robots state)
                          total-time))

(defun day19/evolve-state-resources-to-end (state total-time)
  (if (= (day19-state-time state) total-time)
      state
   (let ((time-resources-evolution (car (last (day19/evolve-state-resources state total-time)))))    
     (make-day19-state :resources (cadr time-resources-evolution)
                       :robots (day19-state-robots state)
                       :time (car time-resources-evolution)))))


(defun day19/eta-for-robot (times-resources cost)
  "ETA for having the robot built (includes building time)"
  (car (cdr (--drop-while (let ((resources (cadr it)))
                        (< (apply #'min (day19/op #'- resources cost)) 0))
                      times-resources))))

(defun day19/jump-state-to-construction (bprint state robot-index total-time)
  (let ((evolved-resources (day19/evolve-state-resources state total-time))
        (robot-cost (elt (day19-bprint-costs bprint) robot-index)))
    (if-let ((expected-deadline (day19/eta-for-robot evolved-resources robot-cost)))
        (let ((resources-after-construction (day19/compute-new-resources (cadr expected-deadline)
                                                                         '(0 0 0 0) ; already accounted
                                                                         robot-cost)))
          (make-day19-state :resources resources-after-construction
                            :robots (-update-at robot-index #'1+ (day19-state-robots state))
                            :time (car expected-deadline))))))

(defun day19/state-score (state total-time)
  (when state
    (elt (day19-state-resources (day19/evolve-state-resources-to-end state total-time)) day19/geo-index)))

(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/best-hypotetical-score-attainable (state max-time)
  (let ((resources (day19-state-resources state))
        (robots (day19-state-resources state))
        (time (day19-state-time state)))
    (+ (elt resources day19/geo-index)
       (* (- max-time time) (elt robots day19/geo-index))
       (let ((dt (- max-time time)))
         (/ (* dt (1- dt)) 2)))))

(defun day19/recursive (bprint state max-time)
  (if (or (>= (day19-state-time state) max-time)
          (<= (day19/best-hypotetical-score-attainable state max-time) day19/*best-result*))
      ;; either we are out of time, or there is no way we make a geo robot in time
      (let ((score (day19/state-score state max-time)))
        (when (> score day19/*best-result*)
          (setq day19/*best-result* score)
;;          (print day19/*best-result*)
;;          (sit-for 0.01)
          )
        score)
    (let* ((robots (day19-state-robots state))
           (costs (day19-bprint-costs bprint))
           (next-moves (cond
                       ;; no clay robots
                       ((zerop (elt robots day19/clay-index)) '(0 1))
                       ;; no obsidian robots
                       ((zerop (elt robots day19/obs-index)) '(0  1 2))
                       ;; Anything goes
                       (t '(3 2 1 0)))))
      (if (>= (elt robots day19/ore-index)
              (apply #'max (-map #'car (cdr costs))))
          (setq next-moves (-remove-item day19/ore-index next-moves)))
      (if (>= (elt robots day19/clay-index)
              (elt (elt costs day19/obs-index) day19/clay-index))
          (setq next-moves (-remove-item day19/clay-index next-moves)))      
      (apply #'max (or (--map (day19/recursive bprint it max-time)
                           (--filter it ; impossible results are not interesting
                                     (--map (day19/jump-state-to-construction bprint state it max-time) next-moves)))
                       (list 0))))))

(defvar day19/*best-result* nil)
(defun day19/start-recursive (bprint state max-time)
  (setq day19/*best-result* 0)
  (let ((result (day19/recursive bprint state max-time)))
    (print (format "%s (%d)-> %d" bprint max-time result))
    result))

(defun day19/compute-quality-level-value (blueprints)
  (apply #'+ (--map (* (day19-bprint-id it)
                      (day19/start-recursive it (day19/create-starting-state) day19/part-1-total-time))
                   blueprints)))

(defun day19/part-1 (lines)
  (day19/compute-quality-level-value (day19/read-problem lines)))


(defun day19/compute-quality-product (blueprints)
  (apply #'* (--map (day19/start-recursive it (day19/create-starting-state) day19/part-2-total-time)
                    blueprints)))

(defun day19/part-2 (lines)
  (day19/compute-quality-product (-take 3 (day19/read-problem lines))))

(provide 'day19)

