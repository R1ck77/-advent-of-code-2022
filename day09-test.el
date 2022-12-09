(require 'day09)
(require 'buttercup)

(describe "--- Day 9: Rope Bridge ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day09/part-1 (advent/read-problem-lines 9 :example))
              :to-be 13))
    (it "solves the problem"
      (expect (day09/part-1 (advent/read-problem-lines 9 :problem))
              :to-be 6522)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day09/part-2 (advent/read-problem-lines 9 :example))
              :to-be 1))
    (it "solves the problem"
      (expect (day09/part-2 (advent/read-problem-lines 9 :problem))
              :to-be 2717))))
