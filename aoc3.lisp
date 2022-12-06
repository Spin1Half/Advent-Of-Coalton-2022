(in-package :coalton-user)



(coalton-toplevel

;;for part 1
  (declare match-first-back (String -> Char))
  
  (define (match-first-back str)
    (let line-len = (string:length str))
    (let halfway = (math:div line-len 2))
    (let str-a = (substring str 0 halfway))
    (let str-b = (substring str halfway line-len))
    (list:car (list:intersection
          (lisp (List Char) (str-a)
            (cl:coerce str-a 'cl:list))
          (lisp (List Char) (str-b)
            (cl:coerce str-b 'cl:list)))))
  
  (declare char-value (Char -> Integer))

  (define (char-value ch)
    (lisp Integer (ch)
      (cl:let ((val (cl:char-int ch)))
        (cl:if (cl:> val 90)
               (cl:- val 96)
               (cl:- val 38)))))


  ;;part2

  (declare group-val (String -> String -> String -> Integer))

  (define (group-val str-a str-b str-c)
    (char-value
     (list:car
      (list:intersection
       (list:intersection
        (lisp (List Char) (str-a)
          (cl:coerce str-a 'cl:list))
        (lisp (List Char) (str-b)
          (cl:coerce str-b 'cl:list)))
       (lisp (List Char) (str-c)
         (cl:coerce str-c 'cl:list)))))))


#+part1
(cl:let ((curr-count 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc3input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (cl:incf curr-count (char-value (match-first-back line)))
      (cl:print line)))
  (cl:print curr-count))


;;part2
(cl:let ((curr-count 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc3input.txt" :direction :input)
    (cl:do* ((line1 (cl:read-line f nil) (cl:read-line f nil))
             (line2 (cl:read-line f nil) (cl:read-line f nil))
             (line3 (cl:read-line f nil) (cl:read-line f nil)))
            ((cl:null line3))
      (cl:incf curr-count (group-val line1 line2 line3))
      (cl:print line1)))
  (cl:print curr-count))










