(require 'day12)
(require 'buttercup)

(describe "--- Day 12: Hill Climbing Algorithm ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day12/part-1 (advent/read-grid 12 :example #'identity))
              :to-be 31 ))
    (it "solves the problem"
      (expect (day12/part-1 (advent/read-grid 12 :problem #'identity))
              :to-be 481)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day12/part-2 (advent/read-grid 12 :example #'identity))
              :to-be 42))
    (xit "solves the problem"
      (expect (day12/part-2 (advent/read-grid 12 :problem #'identity))
              :to-be 42))))
