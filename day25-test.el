(require 'day25)
(require 'buttercup)


(defconst day25-test/examples '((1 . "1")
                                (2 . "2")
                                (3 . "1=")                                               
                                (4 . "1-")
                                (5 . "10")
                                (6 . "11")
                                (7 . "12")
                                (8 . "2=")
                                (9 . "2-")
                                (10 . "20")
                                (15 . "1=0")
                                (20 . "1-0")
                                (2022 . "1=11-2")
                                (12345 . "1-0---0")
                                (314159265 . "1121-1110-1=0")))

(defmacro day25-test/snafu-to-decimal-test (index)
  `(it ,(format "replicates example %d" index)
        (let ((example (elt day25-test/examples ,index)))
          (expect (day25/snafu-to-decimal (cdr example))
                  :to-be (car example)))))

(defmacro day25-test/all-snafu-to-decimal-tests ()
  (let ((code (--map `(day25-test/snafu-to-decimal-test ,it) (number-sequence 0 14))))
    `(progn ,@code)))

(defmacro day25-test/decimal-to-snafu-test (index)
  `(it ,(format "replicates example %d" index)
        (let ((example (elt day25-test/examples ,index)))
          (expect (day25/decimal-to-snafu (car example))
                  :to-equal (cdr example)))))

(defmacro day25-test/all-decimal-to-snafu-tests ()
  (let ((code (--map `(day25-test/decimal-to-snafu-test ,it) (number-sequence 0 14))))
    `(progn ,@code)))

(describe "--- Day 25: Full of Hot Air ---"
  (describe "sub-examples"
    (describe "snafu-to-decimal"
      (day25-test/all-snafu-to-decimal-tests))
    (describe "decimal-to-snafu"
      (day25-test/all-decimal-to-snafu-tests)))  
  (describe "part 1"
    (it "replicates the example"
      (expect (day25/part-1 (advent/read-problem-lines 25 :example))
              :to-be "2=-1=0"))
    (xit "solves the problem"
      (expect (day25/part-1 (advent/read-problem-lines 25 :problem))
              :to-be 42)))
  )
