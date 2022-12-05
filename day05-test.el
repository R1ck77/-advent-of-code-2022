(require 'day05)
(require 'buttercup)

(describe "--- Day 5: Supply Stacks ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day05/part-1 (advent/read-problem-lines 5 :example nil t))
              :to-equal "CMZ" ))
    (it "solves the problem"
      (expect (day05/part-1 (advent/read-problem-lines 5 :problem nil t))
              :to-equal "ZWHVFWQWW")))
  (describe "part 2"
    (it "replicates the example"
      (expect (day05/part-2 (advent/read-problem-lines 5 :example nil t))
              :to-equal "MCD"))
    (it "solves the problem"
      (expect (day05/part-2 (advent/read-problem-lines 5 :problem nil t))
              :to-equal "HZFZCCWWV"))))
