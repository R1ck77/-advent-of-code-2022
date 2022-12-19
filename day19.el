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
           bprint
           time)

(defun day19/create-starting-state (bprint)
  (make-day19-state :time 0
                    :bprint bprint
                    :resources '(0 0 0 0)
                    :robots '(1 0 0 0)))

(defun day19/buildable-robots (resources cost)
  (apply #'min (day19/op (lambda (a b)
                           (if (zerop b)
                               most-positive-fixnum
                             (/ a b)))
                         resources cost)))

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

(defun day19/evolve-with-f (state build-f)
  (let* ((builds (funcall build-f state))
         (current-robots (day19-state-robots state))
         (current-resources (day19-state-resources state))
         (bprint (day19-state-bprint state))
         (costs (day19-bprint-costs bprint))
         (now (day19-state-time state)))
    (-map (lambda (build)
            (let ((costs (day19/build-cost costs build))
                  (gain current-robots))
              (make-day19-state :resources (day19/compute-new-resources current-resources
                                                                        gain
                                                                        costs)
                                :robots (day19/op #'+ current-robots build)
                                :bprint bprint
                                :time (1+ now))))
          builds)))

;;; TODO/FIXME 5 nested cycles. What could go wrong?
(defun day19/possible-builds (state)
  (let* ((resources (day19-state-resources state))
         (costs (day19-bprint-costs (day19-state-bprint state)))
         (ore-cost (elt costs 0))
         (clay-cost (elt costs 1))
         (obs-cost (elt costs 2))
         (geo-cost (elt costs 3))         
         (scenarios))
    ;; I'm assuming the maximum number of geode robots is always the best one
    ;; TODO/FIXME is it true, though?
    (-each (list (day19/buildable-robots resources geo-cost))
     ;-each (number-sequence 0 (day19/buildable-robots resources geo-cost))
      (lambda (geo-robots)
        (let ((resources (day19/sub-mul resources geo-cost geo-robots)))
          (-each (number-sequence 0 (day19/buildable-robots resources obs-cost))
            (lambda (obs-robots)
              (let ((resources (day19/sub-mul resources obs-cost obs-robots)))
                (-each (number-sequence 0 (day19/buildable-robots resources clay-cost))
                  (lambda (clay-robots)
                    (let ((resources (day19/sub-mul resources clay-cost clay-robots)))
                      (-each (number-sequence 0 (day19/buildable-robots resources ore-cost))
                        (lambda (ore-robots)
                          (let ((resources (day19/sub-mul resources ore-cost ore-robots)))
                            (setq scenarios (cons (list ore-robots clay-robots obs-robots geo-robots)
                                                  scenarios))))))))))))))
    scenarios))

;; TODO/FIXME implement if needed
(defun day19/evolve (state)
  (day19/evolve-with-f state #'day19/possible-builds))


(defun day19/geo-robots-builds (state)
  (let* ((resources (day19-state-resources state))
         (costs (day19-bprint-costs (day19-state-bprint state)))
         (geo-cost (elt costs 3))         
        (scenarios))
    (list (list 0 0 0 (day19/buildable-robots resources geo-cost)))))

;; TODO/FIXME implement if needed
(defun day19/evolve-geode-robots (state)
  (day19/evolve-with-f state #'day19/geo-robots-builds))

;; TODO/FIXME implement if needed
(defun day19/evolve-resources (state)
    (day19/evolve-with-f state (lambda (_) '((0 0 0 0)))))

(defun day19/next-scenarios (state)
  "Compute all possibile evolutions of the resources"
  (let ((now (day19-state-time state)))
    (case (- day19/total-time now)
     ;; You messed something up
     (0 (error "Overdue simulation"))
     ;; No time/reason to make more robots: just evolve the resources
     (1 (day19/evolve-resources state))
     ;; No time/reason to create anything but geode extraction robots
     (2 (day19/evolve-geode-robots state))
     ;; Anything goes
     (t (day19/evolve state)))))

(setq *best-result* 0)

(defun day19/compute-quality (state)
  (if (= (day19-state-time state) day19/total-time )
      (let ((result (elt (day19-state-resources state) day19/geo-index)))
        (when (> result *best-result*)
          (setq *best-result* result)
          (print (format "New best: %s" *best-result*))
          (sit-for 0))
        (comment 
         (progn 
           (print (format "Robots: %s Resources: %s Result: %s"
                          (day19-state-robots state)
                          (day19-state-resources state)
                          result))
           (sit-for 0)))
        result)
    (apply #'max (-map #'day19/compute-quality (day19/next-scenarios state)))))

(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(setq e (day19/read-problem example))
(setq p (day19/read-problem problem))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
