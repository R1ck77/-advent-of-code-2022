(require 'day2)
(require 'buttercup)

(describe "Day 2"
  (describe "part 1"
    (it "replicates the example"
      (expect (day2/part-1 (advent/read-problem-lines 2 :example))
              :to-be 15 ))
    (it "solves the problem"
      (expect (day2/part-1 (advent/read-problem-lines 2 :problem))
              :to-be 10941)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day2/part-2 (advent/read-problem-lines 2 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day2/part-2 (advent/read-problem-lines 2 :problem))
              :to-be 42))))
