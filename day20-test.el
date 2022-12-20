(require 'day20)
(require 'buttercup)

(describe "--- Day 20: Grove Positioning System ---"
  (describe "part 1"
    (it "rotates lists as in the example"
      (expect (day20/shift '(1 2 -3 3 -2 0 4) 0 1)
              :to-equal '(2 1 -3 3 -2 0 4))
      (expect (day20/shift '(2 1 -3 3 -2 0 4) 0 2)
              :to-equal '(1 -3 2 3 -2 0 4))
      (expect (day20/shift '(1 -3 2 3 -2 0 4) 1 -3)
              :to-equal '(1 2 3 -2 -3 0 4))
      (expect (day20/shift '(1 2 3 -2 -3 0 4) 2 3)
              :to-equal '(1 2 -2 -3 0 3 4))
      (expect (day20/shift '(1 2 -2 -3 0 3 4) 2 -2)
              :to-equal '(1 2 -3 0 3 4 -2))
      (expect (day20/shift '(1 2 -3 0 3 4 -2) 3 0)
              :to-equal '(1 2 -3 0 3 4 -2))
      (expect (day20/shift '(1 2 -3 0 3 4 -2) 5 4)
              :to-equal '(1 2 -3 4 0 3 -2)))
    (it "replicates the example"
      (expect (day20/part-1 (advent/read-problem-lines 20 :example))
              :to-be 3))
    (it "solves the problem"
      (expect (day20/part-1 (advent/read-problem-lines 20 :problem))
              :to-be 5904)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day20/part-2 (advent/read-problem-lines 20 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day20/part-2 (advent/read-problem-lines 20 :problem))
              :to-be 42))))
