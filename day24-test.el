(require 'day24)
(require 'buttercup)

(describe "--- Day 24: Blizzard Basin ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day24/part-1 (advent/read-problem-lines 24 :example))
              :to-be 18))
    (it "solves the problem"
      (expect (day24/part-1 (advent/read-problem-lines 24 :problem))
              :to-be 295)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day24/part-2 (advent/read-problem-lines 24 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day24/part-2 (advent/read-problem-lines 24 :problem))
              :to-be 42))))
