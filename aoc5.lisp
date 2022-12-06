(in-package :coalton-user)

(ql:quickload "cl-ppcre")


#+ ignore
(
        [H]         [S]         [D]
    [S] [C]         [C]     [Q] [L]
    [C] [R] [Z]     [R]     [H] [Z]
    [G] [N] [H] [S] [B]     [R] [F]
[D] [T] [Q] [F] [Q] [Z]     [Z] [N]
[Z] [W] [F] [N] [F] [W] [J] [V] [G]
[T] [R] [B] [C] [L] [P] [F] [L] [H]
[H] [Q] [P] [L] [G] [V] [Z] [D] [B]
 1   2   3   4   5   6   7   8   9 
 )



(coalton-toplevel

  ;;yes, i really did just type this out
  ;;do i regret it yet [x]
  (declare foo (Vector (Vector Char)))
  (define foo (Vector:make
               (Vector:make)
               (Vector:make #\h #\t #\z #\d)
               (Vector:make #\q #\r #\w #\t #\g #\c #\s)
               (Vector:make #\p #\b #\f #\q #\n #\r #\c #\h)
               (Vector:make #\l #\c #\n #\f #\h #\z)
               (Vector:make #\g #\l #\f #\q #\s)
               (Vector:make #\v #\p #\w #\z #\b #\r #\c #\s)
               (Vector:make #\z #\f #\j)
               (Vector:make #\d #\l #\v #\z #\r #\h #\q)
               (Vector:make #\b #\h #\g #\n #\f #\z #\l #\d)))

  (declare move-one (UFIX -> UFIX -> UFIX))
  (define (move-one from to)
    ;;do i need to be typing optional everywhere i actually do stuff?
    (let el = (Optional:fromsome "" (Vector:pop! (Optional:fromsome "" (Vector:index from foo)))))
    (Vector:push! el (Optional:fromsome "" (Vector:index to foo))))

  (declare move (UFIX -> UFIX -> UFIX -> UNIT))
  (define (move n from to)
      (when (> n 0)
        (move-one from to)
        (move (1- n) from to)))
  
;;part2 (this might be the fastest part2 of all time, took only the
;;function below and changing to move2 in the top routine)
  (declare move2 (UFIX -> UFIX -> UFIX -> UNIT))
  (define (move2 n from to)
    (move n from 0)
    (move n 0 to))

  
)


(cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc5input.txt" :direction :input)
  (cl:do ((line (cl:read-line f nil)
                (cl:read-line f nil)))
         ((cl:null line))
    (ppcre:register-groups-bind  (a b c) ("^move (.*) from (.*) to (.*)$" line)
                                 (cl:print line)
                                 
                                 (move2
                                  (cl:parse-integer a)
                                  (cl:parse-integer b)
                                  (cl:parse-integer c)))))













