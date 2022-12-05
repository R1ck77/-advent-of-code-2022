(require 'day05)
(require 'buttercup)

(describe "Day 5"
  (describe "part 1"
    (it "replicates the example"
      (expect (day05/part-1 (advent/read-problem-lines 5 :example nil t))
              :to-equal "CMZ" ))
    (it "solves the problem"
      (expect (day05/part-1 (advent/read-problem-lines 5 :problem nil t))
              :to-equal "ZWHVFWQWW")))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day05/part-2 (advent/read-problem-lines 5 :example nil t))
              :to-equal 42))
    (xit "solves the problem"
      (expect (day05/part-2 (advent/read-problem-lines 5 :problem nil t))
              :to-equal 42))))
