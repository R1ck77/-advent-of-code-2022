(require 'day15)
(require 'buttercup)

(describe "--- Day 15: Beacon Exclusion Zone ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day15/part-1 (advent/read-problem-lines 15 :example) 10)
              :to-be 26))
    (it "solves the problem"
      (expect (day15/part-1 (advent/read-problem-lines 15 :problem) 2000000)
              :to-be 5240818)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day15/part-2 (advent/read-problem-lines 15 :example) 20)
              :to-be 56000011))
    (it "solves the problem"
      (expect (day15/part-2 (advent/read-problem-lines 15 :problem) 4000000)
              :to-be 13213086906101))))
