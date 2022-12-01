(in-package :coalton-user)

;;

(cl:let ((max-cal 0)
         (curr-cal 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc1input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (cl:if (cl:equal line "")
             (cl:setf max-cal (cl:max max-cal curr-cal)
                      curr-cal 0)
             (cl:incf curr-cal (cl:parse-integer line))))
    (cl:setf max-cal (cl:max max-cal curr-cal) curr-cal 0))
  (cl:print max-cal))



(cl:defun pusht3 (el l)
  (cl:let ((ret (cl:list)))
    (cl:dolist (v l)
      (cl:if (cl:< el v)
             (cl:push v ret)
             (cl:progn (cl:push el ret)
                    (cl:setf el v)))
      )
    ret))

(cl:let ((max-3-cal '(0 0 0))
         (curr-cal 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc1input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (cl:if (cl:equal line "")
             (cl:setf max-3-cal (pusht3 curr-cal max-3-cal)
                      curr-cal 0)
             (cl:incf curr-cal (cl:parse-integer line))))
    (cl:setf max-3-cal (pusht3 curr-cal max-3-cal)))
  (cl:print (cl:apply 'cl:+  max-3-cal)))




