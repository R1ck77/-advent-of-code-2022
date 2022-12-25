(require 'day25)
(require 'buttercup)

(describe "--- Day 25: Full of Hot Air ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day25/part-1 (advent/read-problem-lines 25 :example))
              :to-be 976))
    (xit "solves the problem"
      (expect (day25/part-1 (advent/read-problem-lines 25 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day25/part-2 (advent/read-problem-lines 25 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day25/part-2 (advent/read-problem-lines 25 :problem))
              :to-be 42))))
