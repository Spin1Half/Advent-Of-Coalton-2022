(in-package :coalton-user)

(ql:quickload "cl-ppcre")


(coalton-toplevel

;;for part 1
  (declare nested-interval-p (Integer -> Integer -> Integer -> Integer -> Boolean))
  
  (define (nested-interval-p a b c d)
    (OR (AND (<= a c) (>= b d))
        (AND (>= a c) (<= b d))))

  ;;for part2

  (declare overlap-p (Integer -> Integer -> Integer -> Integer -> Boolean))
  
  (define (overlap-p a b c d)
    (OR (AND (<= a c) (<= c b))
        (AND (<= a d) (<= d b))
        (AND (<= c a) (<= b d))))
  
  )


#+part1
(cl:let ((curr-count 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc4input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (ppcre:register-groups-bind  (a b c d) ("^(.*)-(.*),(.*)-(.*)$" line)
                                   (cl:print line)
                                   (cl:print (cl:parse-integer a))
                                   (cl:print (nested-interval-p 1 2 3 4))
                                   
                                   (cl:when (nested-interval-p
                                              (cl:parse-integer a)
                                              (cl:parse-integer b)
                                              (cl:parse-integer c)
                                              (cl:parse-integer d))
                                          (cl:incf curr-count)
                                     )))
    (cl:print curr-count)))


;;part2
(cl:let ((curr-count 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc4input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (ppcre:register-groups-bind  (a b c d) ("^(.*)-(.*),(.*)-(.*)$" line)
                                   (cl:print line)
                                   ;;(cl:print (cl:parse-integer a))
                                   ;;(cl:print (nested-interval-p 1 2 3 4))
                                   
                                   (cl:when (overlap-p
                                              (cl:parse-integer a)
                                              (cl:parse-integer b)
                                              (cl:parse-integer c)
                                              (cl:parse-integer d))
                                     (cl:print (cl:incf curr-count))
                                     )))
    (cl:print curr-count)))













