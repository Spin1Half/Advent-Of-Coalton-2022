(in-package :coalton-user)

(ql:quickload "cl-ppcre")



(coalton-toplevel

  (define-type Folder
    (Folder (Hashtable String Ufix) (Hashtable String Folder)))
  

  (declare x (Hashtable String Ufix))
  (define x (Hashtable:new))

  
  (define (add-file fold str n)
    (match fold
      ((Folder h1 h2) (hashtable:set! h1 str n))) )


  (define (get-folder fold str)
    (match fold
      ((Folder h1 h2) (match (hashtable:get h2 str)
                        ((Some f) f)
                        ((None) (progn
                                  (hashtable:set! h2 str (Folder (Hashtable:new) (Hashtable:new)))
                                  (unwrap (hashtable:get h2 str))))))))

  (define ht (Folder (Hashtable:new) (Hashtable:new)))

;;i should use zippers here but i am a noob

  (declare add-file-to-path (Folder -> (Optional (List String)) -> String -> Ufix -> Unit))
  (define (add-file-to-path fold lis str n)
    (match lis
      ((Some (Coalton:Nil)) (add-file fold str n))
      ((Some l) (add-file-to-path (get-folder fold (unwrap (list:head l))) (list:tail l) str n))
      
      ))

  (declare size-of-folder (Folder -> Ufix))
  (define (size-of-folder fold)
    (match fold
      ((Folder h1 h2) (+ (size-of-files h1) (list:sum (map size-of-folder (hashtable:values h2)))))
      
      ))

  (declare size-of-files ((Hashtable String Ufix) -> Ufix))
  (define (size-of-files ht)
    (list:sum (hashtable:values ht)))


  ;;im hardcoding this because idk if lambdas exist 
  (declare folders-smaller-than-100000 (Folder -> Ufix))
  (define (folders-smaller-than-100000 fold)
    (match fold
      ( (Folder h1 h2) (+
                      (if (<= (size-of-folder fold) 100000)
                          (size-of-folder fold)
                          0)
                      (list:sum (map folders-smaller-than-100000 (hashtable:values h2)) )))))

  (declare sizes (Vector UFIX))
  (define sizes (vector:new))
  
  
  (define (insert-sizes-bigger-than-8381165 fold)
    (match fold

      ;;you might notice im checking a different number thats because
      ;;at the time im writing this there is WRONG INFORMATION IN THE
      ;;PROMPT (about 1 day after release????) I was able to deduce
      ;;what the actual number should be from context
      ( (Folder h1 h2) (progn (if (>= (size-of-folder fold) 6090134)
                                  (vector:push! (size-of-folder fold) sizes  )
                                  0)
                              (map insert-sizes-bigger-than-8381165 (hashtable:values h2)) )))
    0)


  ;;print stuff for debuging vvv
  (define (pr-kv k v)
    (lisp :t (k v) (cl:format cl:t "~a,~a;" k v)))
  
  (define (pr-ht tab)
    (lisp :t () (cl:format cl:t "["))
    (hashtable:foreach pr-kv tab)
    (lisp :t () (cl:format cl:t "]")))
  
  (define (pr-fold k fold)
    (lisp :t (k) (cl:format cl:t "~a : {" k)
     )
    (match fold
      ((Folder h1 h2) (progn (pr-ht h1) (Hashtable:foreach pr-fold h2)))
      )
    (lisp :t () (cl:format cl:t "}"))
    )



  )

(cl:let ((curr (cl:List))
         (tot 0))
  (cl:with-open-file (f "lisp/advent-of-coalton-2022/aoc7input.txt" :direction :input)
    (cl:do ((line (cl:read-line f nil)
                  (cl:read-line f nil)))
           ((cl:null line))
      (ppcre:register-groups-bind  (a b c file-size file-name) ("^(\[$] cd (.*))|(([0-9]+) (.*))$" (cl:print line))
        (cl:format cl:t "~a" curr )
        (cl:if file-size
               (cl:progn (add-file-to-path ht (some (cl:reverse curr)) file-name (cl:parse-integer file-size))
                         (cl:incf tot (cl:parse-integer file-size))))
        (cl:if b
               (cl:cond 
                 ((cl:equal b "..") (cl:pop curr))
                 ((cl:equal b "/") (cl:setf curr (cl:list)))
                 (cl:t (cl:push b curr)))))))
  (cl:print tot))












