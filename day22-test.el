(require 'day22)
(require 'buttercup)

(describe "--- Day 22: Monkey Map ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day22/part-1 (advent/read-blocks-of-lines 22 :example))
              :to-be 6032))
    (it "solves the problem"
      (expect (day22/part-1 (advent/read-blocks-of-lines 22 :problem))
              :to-be 47462)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day22/part-2 (advent/read-blocks-of-lines 22 :example) :example)
              :to-be 5031))
    (it "solves the problem"
      (expect (day22/part-2 (advent/read-blocks-of-lines 22 :problem) :problem)
              :to-be 137045))))
