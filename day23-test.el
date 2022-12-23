(require 'day23)
(require 'buttercup)

(defconst day23/smaller-example-steps '(
;; Initial state
("....."
"..##."
"..#.."
"....."
"..##."
".....")
;; Step 1
("..##."
"....."
"..#.."
"...#."
"..#.."
".....")
;; Step 2
("....."
"..##."
".#..."
"....#"
"....."
"..#..")
;; Step 3
("..#.."
"....#"
"#...."
"....#"
"....."
"..#..")))

(defconst day23/stripped-initial-state '("##"
                                        "#."
                                        ".."
                                        "##"))

(defun day23-test/strip (state)
  (day23/to-string (day23/read-problem state)))

(describe "--- Day 23: Unstable Diffusion ---"
  (describe "part 1"
    (it "replicates one step of the smaller example"
      (expect (day23/to-string
               (cadr
                (day23/evolve (day23/read-problem (car day23/smaller-example-steps))
                              1)))
              :to-equal (day23-test/strip (elt day23/smaller-example-steps 1))))
    (it "replicates two steps of the smaller example"
      (expect (day23/to-string
               (cadr
                (day23/evolve (day23/read-problem (car day23/smaller-example-steps))
                              2)))
              :to-equal (day23-test/strip (elt day23/smaller-example-steps 2))))
    (it "replicates three steps of the smaller example"
      (expect (day23/to-string
               (cadr
                (day23/evolve (day23/read-problem (car day23/smaller-example-steps))
                              3)))
              :to-equal (s-join "\n" (elt day23/smaller-example-steps 3))))
    (it "replicates the example"
      (expect (day23/part-1 (advent/read-problem-lines 23 :example))
              :to-be 110))
    (it "solves the problem"
      (expect (day23/part-1 (advent/read-problem-lines 23 :problem))
              :to-be 4045)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day23/part-2 (advent/read-problem-lines 23 :example))
              :to-be 20))
    (it "solves the problem"
      (expect (day23/part-2 (advent/read-problem-lines 23 :problem))
              :to-be 963))))
