(require 'day23)
(require 'buttercup)

(defconst day23/smaller-example-steps (list
;; Initial state
".....
..##.
..#..
.....
..##.
....."
;; Step 1
"..##.
.....
..#..
...#.
..#..
....."
;; Step 2
".....
..##.
.#...
....#
.....
..#.."
;; Step 3
"..#..
....#
#....
....#
.....
..#.."))

(describe "--- Day 23: Unstable Diffusion ---"
  (describe "part 1"
    (it "replicates one step of the smaller example"
      (expect (day23/to-string
               (day23/step
                (day23/read-problem (car day23/smaller-example-steps))))
              :to-equal (elt day23/smaller-example-steps 1)))
    (it "replicates two steps of the smaller example"
      (expect (day23/to-string
               (day23/step
                (day23/step
                 (day23/read-problem (car day23/smaller-example-steps)))))
              :to-equal (elt day23/smaller-example-steps 2)))
    (it "replicates three steps of the smaller example"
      (expect (day23/to-string
               (day23/step
                (day23/step
                 (day23/step
                  (day23/read-problem (car day23/smaller-example-steps))))))
              :to-equal (elt day23/smaller-example-steps 3)))
    (it "replicates the example"
      (expect (day23/part-1 (advent/read-problem-lines 110 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day23/part-1 (advent/read-problem-lines 23 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day23/part-2 (advent/read-problem-lines 23 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day23/part-2 (advent/read-problem-lines 23 :problem))
              :to-be 42))))
