(require 'day09)
(require 'buttercup)

(describe "Day 9"
  (describe "part 1"
    (it "replicates the example"
      (expect (day09/part-1 (advent/read-problem-lines 9 :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day09/part-1 (advent/read-problem-lines 9 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day09/part-2 (advent/read-problem-lines 9 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day09/part-2 (advent/read-problem-lines 9 :problem))
              :to-be 42))))