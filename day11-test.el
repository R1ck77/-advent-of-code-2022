(require 'day11)
(require 'buttercup)

(describe "Day 11"
  (describe "part 1"
    (it "replicates the example"
      (expect (day11/part-1 (advent/read-problem-text 11 :example))
              :to-be 10605 ))
    (it "solves the problem"
      (expect (day11/part-1 (advent/read-problem-text 11 :problem))
              :to-be 99840)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day11/part-2 (advent/read-problem-text 11 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day11/part-2 (advent/read-problem-text 11 :problem))
              :to-be 42))))
