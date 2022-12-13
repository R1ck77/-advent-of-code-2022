(require 'day13)
(require 'buttercup)

(describe "Day 13"
  (describe "part 1"
    (it "replicates the example"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :example))
              :to-be 13 ))
    (it "solves the problem"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :problem))
              :to-be 5208)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :example))
              :to-be 140))
    (it "solves the problem"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :problem))
              :to-be 25792))))
