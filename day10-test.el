(require 'day10)
(require 'buttercup)

(describe "--- Day 10: Cathode-Ray Tube ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day10/part-1 (advent/read-problem-lines 10 :example))
              :to-be 13140 ))
    (it "solves the problem"
      (expect (day10/part-1 (advent/read-problem-lines 10 :problem))
              :to-be 14160)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day10/part-2 (advent/read-problem-lines 10 :example))
              :to-equal "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....\n"))
    (it "solves the problem"
      (expect (day10/part-2 (advent/read-problem-lines 10 :problem))
              :to-equal "###....##.####.###..###..####.####..##..\n#..#....#.#....#..#.#..#.#....#....#..#.\n#..#....#.###..#..#.#..#.###..###..#....\n###.....#.#....###..###..#....#....#....\n#.#..#..#.#....#.#..#....#....#....#..#.\n#..#..##..####.#..#.#....####.#.....##..\n"))))
