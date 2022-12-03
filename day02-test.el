(require 'day02)
(require 'buttercup)

(describe "Day 2: Rock Paper Scissors"
  (describe "part 1"
    (it "replicates the example"
      (expect (day02/part-1 (advent/read-problem-lines 2 :example))
              :to-be 15 ))
    (it "solves the problem"
      (expect (day02/part-1 (advent/read-problem-lines 2 :problem))
              :to-be 10941)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day02/part-2 (advent/read-problem-lines 2 :example))
              :to-be 12))
    (it "solves the problem"
      (expect (day02/part-2 (advent/read-problem-lines 2 :problem))
              :to-be 13071))))
