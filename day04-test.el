(require 'day04)
(require 'buttercup)

(describe "Day X"
  (describe "part 1"
    (it "replicates the example"
      (expect (day04/part-1 (advent/read-problem-lines X :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day04/part-1 (advent/read-problem-lines X :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day04/part-2 (advent/read-problem-lines X :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day04/part-2 (advent/read-problem-lines X :problem))
              :to-be 42))))
