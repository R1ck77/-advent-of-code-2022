(require 'day22)
(require 'buttercup)

(describe "Day 22"
  (describe "part 1"
    (it "replicates the example"
      (expect (day22/part-1 (advent/read-blocks-of-lines 22 :example))
              :to-be 6032))
    (xit "solves the problem"
      (expect (day22/part-1 (advent/read-blocks-of-lines 22 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day22/part-2 (advent/read-blocks-of-lines 22 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day22/part-2 (advent/read-blocks-of-lines 22 :problem))
              :to-be 42))))
