(require 'day01)
(require 'buttercup)

(describe "--- Day 1: Calorie Counting ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day01/part-1 (advent/read-blocks-of-numbers 1 :example))
              :to-be 24000))
    (it "solves the problem"
      (expect (day01/part-1 (advent/read-blocks-of-numbers 1 :problem))
              :to-be 69501)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day01/part-2 (advent/read-blocks-of-numbers 1 :example))
              :to-be 45000))
    (it "solves the problem"
      (expect (day01/part-2 (advent/read-blocks-of-numbers 1 :problem))
              :to-be 202346))))
