(require 'day20)
(require 'buttercup)

(describe "--- Day 20: Grove Positioning System ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day20/part-1 (advent/read-problem-lines 20 :example))
              :to-be 3))
    (xit "solves the problem"
      (expect (day20/part-1 (advent/read-problem-lines 20 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day20/part-2 (advent/read-problem-lines 20 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day20/part-2 (advent/read-problem-lines 20 :problem))
              :to-be 42))))
