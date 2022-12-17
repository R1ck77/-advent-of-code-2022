(require 'day17)
(require 'buttercup)

(describe "--- Day 17: Pyroclastic Flow ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day17/part-1 (advent/read-problem-text 17 :example))
              :to-be 3068))
    (xit "solves the problem"
      (expect (day17/part-1 (advent/read-problem-text 17 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day17/part-2 (advent/read-problem-text 17 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day17/part-2 (advent/read-problem-text 17 :problem))
              :to-be 42))))
