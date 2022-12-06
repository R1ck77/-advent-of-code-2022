(require 'day06)
(require 'buttercup)

(describe "--- Day 6: Tuning Trouble ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day06/part-1 (advent/read-problem-lines 6 :example))
              :to-equal '(7 5 6 10 11)))
    (it "solves the problem"
      (expect (day06/part-1 (advent/read-problem-lines 6 :problem))
              :to-equal '(1816))))
  (describe "part 2"
    (it "replicates the example"
      (expect (day06/part-2 (advent/read-problem-lines 6 :example))
              :to-equal '(19 23 23 29 26)))
    (it "solves the problem"
      (expect (day06/part-2 (advent/read-problem-lines 6 :problem))
              :to-equal '(2625)))))
