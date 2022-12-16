(require 'dash)
(require 'advent-utils)
(require 's)

(defconst example (advent/read-problem-lines 16 :example))
(defconst problem (advent/read-problem-lines 16 :problem))

(defstruct day16-valve "Valve definition"
           name
           flow
           tunnels)

(defun day16/read-tunnels (raw-tunnels)
  (--map (intern (s-append it ":")) (s-split ", " raw-tunnels)))

(defun day16/read-valve (line)
  (let ((matches (s-match "Valve \\(..\\) has flow rate=\\(.+\\); tunnels? leads? to valves? \\(.+\\)"
                          line)))
    (seq-let (_ name flow raw-tunnels) matches
      (make-day16-valve :name (intern (s-append name ":"))
                        :flow flow
                        :tunnels (day16/read-tunnels raw-tunnels)))))

(defun day16/read-problem (lines)
  (-map #'day16/read-valve lines))

(defun day16/part-1 (lines)
  (error "Not yet implemented"))

(defun day16/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day16)
