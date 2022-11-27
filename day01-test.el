(require 'day01)
(require 'buttercup)

(describe "--- Day 1: Sonar Sweep ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day01/part-1 (advent/read-problem-numbers 1 :example))
              :to-be 7))
    (it "solves the problem"
      (expect (day01/part-1 (advent/read-problem-numbers 1 :problem))
              :to-be 1233)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day01/part-2 (advent/read-problem-numbers 1 :example))
              :to-be 5))
    (it "solves the problem"
      (expect (day01/part-2 (advent/read-problem-numbers 1 :problem))
              :to-be 1275))))
