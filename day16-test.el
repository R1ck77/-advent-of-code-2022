(require 'day16)
(require 'buttercup)

(describe "--- Day 16: Proboscidea Volcanium ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day16/part-1 (advent/read-problem-lines 16 :example))
              :to-be 1651 ))
    (it "solves the problem"
      (expect (day16/part-1 (advent/read-problem-lines 16 :problem))
              :to-be 1559)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day16/part-2 (advent/read-problem-lines 16 :example))
              :to-be 1707))
    (it "solves the problem"
      (expect (day16/part-2 (advent/read-problem-lines 16 :problem))
              :to-be 2191))))
