(in-package :coalton-user)


;; "If you can hardcode it in 9 lines, you might as well" -Parker AOC 2022

;;
#+part1
(coalton-toplevel
  (declare value (String -> Integer))
  
  (define (value str)
    (cond
      ((== str "A X") (+ 1 3))
      ((== str "A Y") (+ 2 6))
      ((== str "A Z") (+ 3 0))
      ((== str "B X") (+ 1 0))
      ((== str "B Y") (+ 2 3))
      ((== str "B Z") (+ 3 6))
      ((== str "C X") (+ 1 6))
      ((== str "C Y") (+ 2 0))
      ((== str "C Z") (+ 3 3))
      (True (Error "@@@@@@@@@@@@")))))


(coalton-toplevel
  (declare value (String -> Integer))
  
  (define (value str)
    (cond
      ((== str "A X") (+ 3 0))
      ((== str "A Y") (+ 1 3))
      ((== str "A Z") (+ 2 6))
      ((== str "B X") (+ 1 0))
      ((== str "B Y") (+ 2 3))
      ((== str "B Z") (+ 3 6))
      ((== str "C X") (+ 2 0))
      ((== str "C Y") (+ 3 3))
      ((== str "C Z") (+ 1 6))
      (True (Error "@@@@@@@@@@@@")))))

(cl:let ((curr-score 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc2input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (cl:incf curr-score (value line))
      (cl:print line)))
  (cl:print curr-score))










