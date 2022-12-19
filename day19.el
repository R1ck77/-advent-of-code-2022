(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 19 :example))
(defconst problem (advent/read-problem-lines 19 :problem))

(defconst day19/total-time 24)

(defstruct day19-bprint "Blueprint description"
           id
           ore-robot
           clay-robot
           obsidian-robot
           geode-robot)

(defun day19/read-geode-robot (definition)
  (seq-let (_ s-ore s-obsidian)
      (s-match "Each geode robot costs \\(.+\\) ore and \\(.+\\) obsidian"
               definition)
    (list (string-to-number s-ore)
          (string-to-number s-obsidian))))

(defun day19/read-obsidian-robot (definition)
  (seq-let (_ s-ore s-clay)
      (s-match "Each obsidian robot costs \\(.+\\) ore and \\(.+\\) clay"
               definition)
    (list (string-to-number s-ore)
          (string-to-number s-clay))))

(defun day19/read-clay-robot (definition)
  (string-to-number
   (cadr
    (s-match "Each clay robot costs \\(.+\\) ore"
             definition))))

(defun day19/read-ore-robot (definition)
  (string-to-number
   (cadr
    (s-match "Each ore robot costs \\(.+\\) ore"
             definition))))

(defun day19/read-id (definition)
  (string-to-number
   (cadr
    (s-match "Blueprint \\(.+\\)"
             definition))))

(defun day19/read-blueprint (line)
  (seq-let (raw-bprint-id
            raw-ore-robot
            raw-clay-robot
            raw-obsidian-robot
            raw-geode-robot)
      (split-string line "[.:] ?" t)
    (make-day19-bprint :id (day19/read-id raw-bprint-id)
                       :ore-robot (day19/read-ore-robot raw-ore-robot)
                       :clay-robot (day19/read-clay-robot raw-clay-robot)
                       :obsidian-robot (day19/read-obsidian-robot raw-obsidian-robot)
                       :geode-robot (day19/read-geode-robot raw-geode-robot))))

(defun day19/read-problem (lines)
  (-map #'day19/read-blueprint lines))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
