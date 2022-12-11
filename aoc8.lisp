(in-package :coalton-user)





(coalton-toplevel

  (declare data (Vector (Vector (Vector Integer))))
  (define data (Vector:new))


  ;;call with col 1 and row 0
  (define (compute-right dat row col)
    (if (>= row (Vector:length dat))
        0
        (if (>= col (Vector:length (unwrap (vector:index 0 dat))))
            (compute-right dat (1+ row) 1)
            (progn
              (let r = (unwrap (Vector:index row dat)))
              (let el = (unwrap (Vector:index col r)))
              (Vector:set! 1
                           (max  (unwrap (Vector:index 0 (unwrap (Vector:index (1- col) r))))
                                 (unwrap (Vector:index 1 (unwrap (Vector:index (1- col) r)))))
                           el )
              (compute-right dat row (1+ col))))))

  ;;call with col 0 and row 1
  (define (compute-down dat row col)
    (if (>= row (Vector:length dat))
        0
        (if (>= col (Vector:length (unwrap (vector:index 0 dat))))
            (compute-down dat (1+ row) 0)
            (progn
              (let r = (unwrap (Vector:index row dat)))
              (let r-- = (unwrap (Vector:index (1- row) dat)))
              (let el = (unwrap (Vector:index col r)))
              (Vector:set! 2 (max (unwrap (Vector:index 0 (unwrap (Vector:index col r--))))
                                  (unwrap (Vector:index 2 (unwrap (Vector:index col r--)))))
                           el )
              (compute-down dat row (1+ col))))))

  ;;call with col 98 and row 97
  (define (compute-up dat row col)
    ;;row has to overflow, if i make it an int then i cant use it to index vectors..
    (if (> row 99)
        0
        (if (> col 99)
            (compute-up dat (1- row) 98
                        )
            
            (progn
              (let r = (unwrap (Vector:index row dat)))
             
              (let r++ = (unwrap (Vector:index (1+ row) dat)))
              (let el = (unwrap (Vector:index col r)))
              (Vector:set! 3 (max (unwrap (Vector:index 0 (unwrap (Vector:index col r++))))
                                  (unwrap (Vector:index 3 (unwrap (Vector:index col r++)))))
                           el )
              (compute-up dat row (1- col))
              ))))

  ;;call with col 97 and row 98
  (define (compute-left dat row col)
    (if (> row 99)
        0
        (if (> col 99)
            (compute-left dat (1- row) 97
                          )
            (progn
              (let r = (unwrap (Vector:index row dat)))

              (let el = (unwrap (Vector:index col r)))
              (Vector:set! 4 (max (unwrap (Vector:index 0 (unwrap (Vector:index (1+ col) r))))
                                  (unwrap (Vector:index 4 (unwrap (Vector:index (1+ col) r)))))
                           el )
              (compute-left dat row (1- col)))))
    )
  
  (define (sum-row dat)
    (list:sum (map is-vis (the (List (Vector Integer)) (into dat)))))

  (define (compute-all dat)
    (compute-right dat 0 1)
    (compute-down dat 1 0)
    ;;(compute-up dat 3 4)
    ;;(compute-left dat 4 3)
    (compute-up dat 97 98)
    (compute-left dat 98 97)
    (list:sum (map sum-row (the (List (Vector (Vector Integer))) (into dat))))
    )

  (declare is-vis ((Vector Integer) -> Ufix))
  (define (is-vis cell)
    (let h = (unwrap (vector:index 0 cell)))
    (if (OR (> h (unwrap (vector:index 1 cell)))
            (> h (unwrap (vector:index 2 cell)))
            (> h (unwrap (vector:index 3 cell)))
            (> h (unwrap (vector:index 4 cell))))
        1
        0))


  (define (height dat row col)
    (let r = (unwrap (Vector:index row dat)))
    (let c = (unwrap (Vector:index col r)))
    (unwrap (vector:index 0 c)))
  
  (define (viewing-score dat row col)
    (let h = (height dat row col))
    (* (* (* (up-score dat (1- row) col h)
             (down-score dat (1+ row) col h))
          (right-score dat row (1+ col) h))
       (left-score dat row (1- col) h))
      )

  (define (up-score dat row col h)
    (if (> row 99)
        0
        (if (>= (height dat row col) h)
            1
            (+ 1
               (up-score dat (1- row) col h)))))


  (define (down-score dat row col h)
    (if (> row 98)
        0
        (if (>= (height dat row col) h)
            1
            (+ 1
               (down-score dat (1+ row) col h)))))


  (define (right-score dat row col h)
    (if (>= col 99)
        0
        (if (>= (height dat row col) h)
            1
            (+ 1
               (right-score dat row (1+ col) h)))))


  (define (left-score dat row col h)
    (if (> col 99)
        0
        (if (>= (height dat row col) h)
            1
            (+ 1
               (left-score dat row (1- col) h)))))


  (define (max-viewing-score dat row col)
    (if (> col 98)
        (max-viewing-score dat (1+ row) 0)
        (if (> row 98)
            0
            (max (viewing-score dat row col)
                 (max-viewing-score dat row (1+ col))))))


  )

(cl:let ((i 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc8input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (Coalton (vector:push! (Vector:new) data))
      (cl:loop for c across line do
        (cl:let ((num (cl:digit-char-p c)))
          (Vector:push!
           (cl:vector num -1 -1 -1 -1)
           (Coalton (unwrap(Vector:last data))))))
      (cl:incf i)
      
      ))
  )












