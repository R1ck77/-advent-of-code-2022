(require 'day19)
(require 'buttercup)

(describe "--- Day 19: Not Enough Minerals ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day19/part-1 (advent/read-problem-lines 19 :example))
              :to-be 33))
    (it "solves the problem"
      (expect (day19/part-1 (advent/read-problem-lines 19 :problem))
              :to-be 1653)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day19/part-2 (advent/read-problem-lines 19 :example))
              :to-be 42))
    (it "solves the problem"
      (expect (day19/part-2 (advent/read-problem-lines 19 :problem))
              :to-be 42))))
