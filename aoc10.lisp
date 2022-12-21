(in-package :coalton-user)


(ql:quickload "cl-ppcre")


(coalton-toplevel

  (declare v (Vector Integer)) ;;ith index corresponds to val of X at the i-1th clock cycle
  (define v (vector:make))
  (declare x (Cell Integer))
  (define x (Cell:New 1)) ;;

  
  (define-type instr
    NOOP
    (ADDX Integer))

  
  (define (do-instr inst)
    (match inst
      ((NOOP) (progn (Vector:push! (Cell:read x) v) Unit))
      ((ADDX val) (progn (Vector:push! (Cell:read x) v)
                         (Vector:push! (Cell:read x) v)
                         (Cell:write! x (+ (Cell:read x) val))
                         Unit))))

  (define (do-addx val)
    (do-instr (ADDX val)))

  
  (define (do-noop)
    (do-instr NOOP))

  (declare signal-strength (UFIX -> Integer))
  (define (signal-strength ind)
    (* (the (Integer) (into ind)) (vector:index-unsafe (1- ind) v))
    )
 
  (define (the-sum i)
      (if (== i 20)
          (signal-strength 20)
          (+ (signal-strength i) (the-sum (- i 40)))))

  (define (print-row n)
    (gen-row (* 40 n) (* 40 (1+ n))))
  

  (define (gen-row begin end)
    (if (== begin end)
        ""
        (String:Concat (if (> 2 (abs (- (mod (the (Integer) (into begin)) 4) (vector:index-unsafe begin v))))
                           "#"
                           ".")
                       (gen-row (1+ begin) end))
        )
    )


  )


(cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc10input.txt" :direction :input)
  (cl:do ((line (cl:read-line f nil) 
                (cl:read-line f nil)))
         ((cl:null line))
    ;;(cl:print line)
    (ppcre:register-groups-bind  (a b) ("^([a-z]+)( .*)?$" line)
      
      (cl:if (cl:equal a "noop")
             (coalton (do-noop))
             (cl:let ((parsed-b (cl:parse-integer b)))
               (coalton (do-addx (lisp :t () parsed-b)))))))
  
  (cl:dolist (x '(0 1 2 3 4 5))
    (cl:print (coalton (print-row (lisp :t () x)))))
  )













