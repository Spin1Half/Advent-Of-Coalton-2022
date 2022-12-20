(in-package :coalton-user)


(ql:quickload "cl-ppcre")


(coalton-toplevel

  (define-type coord
    (coord Integer Integer))

  (define (coordx cor)
    (match cor
      ((Coord x _) x)))

  (define (coordy cor)
    (match cor
      ((Coord _ y) y)))

  (define-instance (Eq coord)
    (define (== a b)
      (AND
       (== (coordx a) (coordx b))
       (== (coordy a) (coordy b)))))

  (define-type rope
    (rope coord coord))

  ;;dont ask
  (define-type direction
    (U String)
    (D String)
    (L String)
    (R String))


  ;;part2

  (define starting-chain (vector:make
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))
                          (rope (coord 0 0) (coord 0 0))))
  

  (define (dir-to-dir s)
    (cond
      ((== "U" s) (U ""))
      ((== "D" s) (D ""))
      ((== "R" s) (R ""))
      (True (L "")))
    )

  

  (define starting-rope (rope (coord 0 0) (coord 0 0)))

  (define tail-positions (vector:make (coord 0 0)))

  (define chain-tail-positions (vector:make (coord 0 0)))

  
  
  (define (move-head dir rop)
    (match rop
      ((rope (coord hx hy) (coord tx ty))
       (match dir
         ((U Nil) (rope (coord hx (1+ hy)) (coord tx ty)))
         ((D Nil) (rope (coord hx (1- hy)) (coord tx ty)))
         ((L Nil) (rope (coord (1- hx) hy) (coord tx ty)))
         ((R Nil) (rope (coord (1+ hx) hy) (coord tx ty)))))))

  (define (snap-tail-old rop)
    (match rop
      ((rope (coord hx hy) (coord tx ty))
       (if (< 1 (- hx tx))
           (rope (coord hx hy) (coord (1- hx) hy))
           (if (> -1 (- hx tx))
               (rope (coord hx hy) (coord (1+ hx) hy))
               (if (< 1 (- hy ty))
                   (rope (coord hx hy) (coord hx (1- hy)))
                   (if (> -1 (- hy ty))
                       (rope (coord hx hy) (coord hx (1+ hy)))
                       (rope (coord hx hy) (coord tx ty)))))))))

  (define (snap-tail rop)
    (match rop
      ((rope (coord hx hy) (coord tx ty))
       (progn
         (let delta-x = (- hx tx))
         (let delta-y = (- hy ty))
         (if (or (< 1 (abs delta-x)) (< 1 (abs delta-y)))
             (progn (let step-x = (min (max -1 delta-x) 1))
                    (let step-y = (min (max -1 delta-y) 1))
                    (rope (coord hx hy) (coord (+ step-x tx) (+ step-y ty))))
             
             rop
             )

         )

       )))

  (define (tail-pos rop)
    (match rop
      ((rope (Coord _ _) (coord x y)) (coord x y)) ))

  (define (move dir)
    
    (let new-rope = (snap-tail (move-head dir starting-rope)))
    (lisp :t (new-rope) (cl:print new-rope) )
    (Vector:push! (tail-pos new-rope) tail-positions)
    (lisp :t (new-rope) (cl:setf starting-rope new-rope)))

  
  (define (tail-chain chain)
    (match (vector:last-unsafe chain)
      ((rope (coord x y) (coord _ _)) (coord x y)))
    )
  
  ;;call initially at 0
  
  (define (move-chain dir ind)
    (if (> ind 9)
        (Vector:push! (tail-chain starting-chain) chain-tail-positions)
        (if (== ind 0)
            (progn
              (let kn = (vector:index-unsafe 0 starting-chain))
              (vector:set! 0 (snap-tail (move-head dir kn)) starting-chain)
              (move-chain dir 1))
            (progn
              (let kn = (vector:index-unsafe (1- ind) starting-chain))
              
              (vector:set! ind (snap-tail (match kn
                                            ((Rope (coord _ _) (coord x y)) (Rope (coord x y) (match (vector:index-unsafe ind starting-chain)
                                                                                                ((Rope (coord _ _) (coord x y)) (coord x y)))))))
                           starting-chain)
              (move-chain dir (1+ ind))
              )
            ))
    )

  (define (move-chain-n dir n)
    (if (== 1 n)
        (move-chain dir 0)
        (progn
          (move-chain dir 0)
          (move-chain-n dir (1- n)))
        ))
  
  (define (move-n dir n)
    (if (== n 1)
        (move dir)
        (progn
          (move dir)
          (move-n dir (1- n)))))


 

  )


(cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc9input.txt" :direction :input)
  (cl:do ((line (cl:read-line f nil)
                (cl:read-line f nil)))
         ((cl:null line))
    (cl:print line)
    (ppcre:register-groups-bind  (a b) ("^([A-Z]) (.*)$" line)
                                 (cl:let ((int-b (cl:parse-integer b)))
                                   (coalton (move-chain-n (dir-to-dir (lisp String () a))
                                                    
                                                    (lisp Integer () int-b)
                                                    )))))
  (cl:print (coalton (list:length (list:remove-duplicates (the (List coord) (into chain-tail-positions))))))
  )













