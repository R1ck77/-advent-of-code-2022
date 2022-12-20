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
           blocks
           bprint
           time)

(defun day19/create-starting-state (bprint)
  (make-day19-state :time 0
                    :bprint bprint
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)
                    :blocks '(nil nil nil nil)))

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

;; TODO/FIXME assumes that the factory cannot create more than one robot per minute
(defun day19/evolve-with-f (state evolutions-f)
  (let* ((evolutions (funcall evolutions-f state))
         (current-robots (day19-state-robots state))
         (current-resources (day19-state-resources state))
         (bprint (day19-state-bprint state))
         (costs (day19-bprint-costs bprint))
         (now (day19-state-time state)))
    (-map (lambda (evolution)
            (let* ((new-robot (day19-evolution-new-robot evolution))
                   (costs (day19/build-cost costs new-robot))
                   (gain current-robots))
              (comment
               (print (day19/compute-new-resources current-resources gain costs))
               (sit-for 0.01))
              (make-day19-state :resources (day19/compute-new-resources current-resources
                                                                        gain
                                                                        costs)
                                :robots (day19/op #'+ current-robots new-robot)
                                :blocks (day19-evolution-next-blocks evolution)
                                :bprint bprint
                                :time (1+ now))))
          evolutions)))

(defun day19/get-buildable-robots (state)
  "Returns a list of robots that can be built"
  (let ((resources (day19-state-resources state))
        (costs (day19-bprint-costs (day19-state-bprint state))))
    (--map (day19/is-buildable? resources (elt costs it)) (number-sequence 0 3))))

(defstruct day19-evolution "Developments during the simulation"
           next-blocks
           new-robot)

(defun day19/remove-blocked-moves (buildable blocks)
  "Remove the buildable robots that are currently blocked"
  (--map (if (car it) 0 (cdr it)) (-zip blocks buildable)))

(defun day19/create-move (index)
  (-replace-at index 1 '(0 0 0 0)))

(defun day19/add-block (blocks index)
  (-replace-at index t blocks))

;; TODO/FIXME not sure if building a geode robot is mandatory, I'll play it safe for now
(defun day19/possible-evolutions (state)
  "Returns all possible developments of the current state"
  (let ((buildable (day19/get-buildable-robots state))
        (current-blocks (day19-state-blocks state)))    
    (let ((possible-moves (day19/remove-blocked-moves buildable current-blocks)))
      (if (not (equal possible-moves '(nil nil nil nil)))
       ;; I can do something
       (--mapcat
        (when (elt possible-moves it)
          (list
           ;; build and clear the current blocks
           (make-day19-evolution :next-blocks '(nil nil nil nil)
                                 :new-robot (day19/create-move it))
           ;; don't build and add a block
           (make-day19-evolution :next-blocks (day19/add-block current-blocks it)
                                 :new-robot '(0 0 0 0))))
        '(0 1 2 3))
       ;; There is no available move:just carry on
       (list (make-day19-evolution :next-blocks current-blocks
                                   :new-robot '(0 0 0 0)))))))

(defun day19/evolve (state)
  (day19/evolve-with-f state #'day19/possible-evolutions))

(defun day19/evolve-resources (state)
  (day19/evolve-with-f state (lambda (_)
                               (list (make-day19-evolution :next-blocks '(nil nil nil nil)
                                                           :new-robot '(0 0 0 0))))))

(defun day19/next-scenarios (state)
  "Compute all possibile evolutions of the resources"
  (let ((now (day19-state-time state)))
    (case (- day19/total-time now)
     ;; You messed something up
     (0 (error "Overdue simulation"))
     ;; No time/reason to make more robots: just evolve the resources
     (1 (day19/evolve-resources state))
     ;; Anything goes
     (t (day19/evolve state)))))

(setq *best-result* 0)

(defun day19/is-absurd? (state)
  (let ((blocks (day19-state-blocks state))
        (robots (day19-state-robots state)))
    (or
     ;; everything is blocked
     (equal blocks '(t t t t))
     ;; no clay robots…
     (and (zerop (elt robots day19/clay-index))
          ;; but both ore and clay robots cannot be built
          (elt blocks day19/ore-index)
          (elt blocks day19/clay-index))
     ;; no obsidian robots
     (and (zerop (elt robots day19/obs-index))
          ;; but only the geode robot can be built
          (equal blocks '(t t t nil))))))

(defun day19/compute-quality (state)
  (cond
   ((day19/is-absurd? state)
    -1)
   ((= (day19-state-time state) day19/total-time)
    (let ((result (elt (day19-state-resources state) day19/geo-index)))
      (comment
        (print (format "RESOURCES: %s ROBOTS: %s BLOCKS: %s"
                       (day19-state-resources state)
                       (day19-state-robots state)
                       (day19-state-blocks state)))
        (sit-for 0.1))
      (when (> result *best-result*)
        (setq *best-result* result)
        (print (format "New best: %s" *best-result*))
        (sit-for 0))
      result))
   (t (apply #'max (-map #'day19/compute-quality (day19/next-scenarios state))))))

(defun day19/compute-blueprint-quality (bprint)
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
