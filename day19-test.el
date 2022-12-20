(require 'day19)
(require 'buttercup)

(describe "--- Day 19: Not Enough Minerals ---"
  (describe "part 1"
    (describe "sub-problems"
      (it "finds the correct quality for the first example blueprint"
        (expect (day19/compute-blueprint-quality (elt (day19/read-problem (advent/read-problem-lines 19 :example)) 0))
                :to-be 9))
      (it "finds the correct quality for the second example blueprint"
        (expect (day19/compute-blueprint-quality (elt (day19/read-problem (advent/read-problem-lines 19 :example)) 1))
                :to-be 12)))
    (xit "replicates the example"
      (expect (day19/part-1 (advent/read-problem-lines 19 :example))
              :to-be 33))
    (xit "solves the problem"
      (expect (day19/part-1 (advent/read-problem-lines 19 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day19/part-2 (advent/read-problem-lines 19 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day19/part-2 (advent/read-problem-lines 19 :problem))
              :to-be 42))))
