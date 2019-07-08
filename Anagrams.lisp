;;;
;;;
;;;

(defparameter *lexican-pathname* NIL)
(defparameter *anagrams-pathname* NIL)
(defparameter *number-to-words-map* NIL)

(defparameter *letter-freqs* 
  '((#\. .  1)
    (#\, .  1)
    (#\' .  1)
    (#\" .  1)
    (#\- .  1)
    (#\& .  1)
    (#\! .  1)
    (#\/ .  1)
    (#\e .  2)
    (#\t .  3)
    (#\a .  5)
    (#\o .  7)
    (#\i . 11)
    (#\n . 13)
    (#\s . 17)
    (#\h . 19)
    (#\r . 23)
    (#\d . 29)
    (#\l . 31)
    (#\c . 37)
    (#\u . 41)
    (#\m . 43)
    (#\w . 47)
    (#\f . 53)
    (#\g . 59)
    (#\y . 61)
    (#\p . 67)
    (#\b . 71)
    (#\v . 73)
    (#\k . 79)
    (#\j . 83)
    (#\x . 89)
    (#\q . 97)
    (#\z . 101)
    (#\0 . 103)
    (#\1 . 107)
    (#\2 . 109)
    (#\3 . 113)
    (#\4 . 127)
    (#\5 . 131)
    (#\6 . 137)
    (#\7 . 139)
    (#\8 . 149)
    (#\9 . 151)))


(defun parse-config-file (s)
  (let*
      ((ht-cnfg    (yaml:parse (pathname s)))
       (lxcn       (gethash "lexican" ht-cnfg))
       (lxcn-flnm  (gethash "filename" lxcn))
       (lxcn-extn  (gethash "extension" lxcn))
       (lxcn-pth   (gethash "path" lxcn))
       (lxcn-drctr (gethash "directory" lxcn))
       (angrms       (gethash "anagrams" ht-cnfg))
       (angrms-flnm  (gethash "filename" angrms))
       (angrms-extn  (gethash "extension" angrms))
       (angrms-pth   (gethash "path" angrms))
       (angrms-drctr (gethash "directory" angrms)))
    (setf *lexican-pathname*
	  (namestring
	   (make-pathname
	    :name lxcn-flnm
	    :type lxcn-extn
	    :directory `(:relative ,lxcn-pth ,lxcn-drctr))))
    (setf *anagrams-pathname*
	  (namestring
	   (make-pathname
	    :name angrms-flnm
	    :type angrms-extn
	    :directory `(:relative ,angrms-pth ,angrms-drctr))))
    (format t "*lexican-pathname* = ~S~%" *lexican-pathname*)
    (format t "*anagrams-pathname* = ~S~%" *anagrams-pathname*)
    (format t "~%"))
  'DONE)

(defun rem-punctuation (s)
  "Takes an arbitrary string (s), and returns it sans any punctuation."
  (remove-if
   #'(lambda (x) (member x '(#\. #\, #\' #\" #\? #\! #\- #\/ #\Space)))
   s))


(defun word->number (w)
  "Takes a single word (w) as a string, and returns the number obtained by
   multiplying together the primes corresponding to each letter in w."
  (reduce #'* (loop for i across w collect (cdr (assoc i *letter-freqs*)))))


(defun save-in-file (o p)
  "Takes an object (o) and a pathname (p) to write to, and saves o to f."
  (with-open-file (strm
		   p
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    ;; (format strm "*lexican-pathname* = ~S~%" *lexican-pathname*)
    ;; (format strm "*anagrams-pathname* = ~S~%" *anagrams-pathname*)
    (format strm "~S" o)))


(defun map-numbers-to-words (from to)
  "Takes a string (p) denoting the path of a file, and returns a hash-table 
   having keys as numbers and values as lists of anagrams."
  (setf *number-to-words-map* (make-hash-table))
  (with-open-file (fl-strm from)
    (when fl-strm 
      (loop
	 for ln = (read-line fl-strm NIL)
	 while ln
	   
	 do (let* ((word (string-downcase (rem-punctuation ln)))
		   (number (word->number word))
		   (words (gethash number *number-to-words-map*)))
	      (if (null words)
		  (progn
		    (let ((ws (gethash number *number-to-words-map*))
			  (ws-new
			   (remove-duplicates (append (list word) NIL)
					      :test #'equal)))
		      (setf (gethash number *number-to-words-map*) ws-new)
		      ;;(format t "No words:  ~s ~s ~s ~s~%"
		      ;;word number ws ws-new)
		      ))
		  (progn
		    (let ((ws (gethash number *number-to-words-map*))
			  (ws-new
			   (remove-duplicates (append words (list word))
					      :test #'equal)))
		      (setf (gethash number *number-to-words-map*) ws-new)
		      ;;(format t "Words:     ~s ~s ~s ~s~%"
		      ;;word number ws ws-new)
		      )))))))
  (save-in-file (alexandria:hash-table-alist *number-to-words-map*) to))

(defun fetch-from-file (p)
  "Takes a pathname (p), and returns the contents of file denoted by p."
  (with-open-file (strm
		   p
		   :if-does-not-exist :error)
    (when strm
      (read strm))))

(defun show-alist (l)
  "Takes an association list (l), and prints out its entries one per line plainly
   i.e without quotes"
  (format t "~%angrms:~%")
  (loop for e in l
     do
       (loop for a in e
	  do
	    (format t " ~a" a))
       (format t "~%"))
  (format t "~%"))

(defun anagrams (s p)
  "Takes a string (s) and a pathname (p), and returns the anagrams of s from the
   file denoted by p."
  (let* ((number (word->number s))
 	 (ht-numbers-to-words (alexandria:alist-hash-table (fetch-from-file p))))
    (gethash number ht-numbers-to-words)))


(defun flatten (l)
  "Takes an arbitrarily-nested list (l), and returns all its elements in a 
   single-level list."
  (cond ((null l) NIL)
	((atom l) (list l))
	(t (append (flatten (car l)) (flatten (cdr l))))))


(defun anagrams-2 (p l)
  "Takes a phrase (p) and a length (l), and returns the anagrams of all the 
   subwords of p where each subword is atleast l characters long."
  (format t "defun:  p ~s, l ~s~%" p l)
  
  (let* ((p2     (rem-punctuation p))
	 (l2     (length p2))
	 (angrms ())
	 (rslt ()))
    (format t "defun:  p2 ~S, l2 ~s~%" p2 l2)
    
    (labels
	
	((helper (alist s i)
	   (format t "~%")
	   (format t "helper:            alist ~s, s ~s, i ~a ~%" alist s i)

	   (if (< i l2)
	       (progn
		 (setf rslt alist)
		 (let ((rslt alist)
		       (as ())
		       (e ())
		       (rslt-2 ())
		       (s2 ())
		       (s3 ()))

		   (if (< (length s) l)
		       (progn
			 (setf as (anagrams s *anagrams-pathname*))
			 (format t "(< (length s) l):  as ~s~%" as)
			 (loop for a in as
			    do
			      (progn
				(format t "loop :                a ~s~%" a)
				(if alist
				    (loop for e2 in alist
				       do
					 (progn
					   (setf e (append e2 (list a)))
					   (setf rslt-2 (remove e2 rslt :test #'equal))
					   (setf rslt rslt-2)
					   (push e rslt)
					   (format
					    t
					    "loop loop:        e2 ~s, e ~s, rslt ~s~%"
					    e2 e rslt)))
				    (progn
				      (push (list a) rslt)
				      (format t "loop loop:         rslt ~s~%" rslt)))))
			 (helper rslt (alexandria:rotate p2 1) (+ i 1)))
		       
		       (progn
			 (setf rslt alist)
			 (setf s2 (subseq s 0 l))
			 (setf s3 (subseq s l))
			 (setf as (anagrams s2 *anagrams-pathname*))
			 (format t "(>= (length s) l):  s2 ~S, s3 ~S, as ~S~%" s2 s3 as)
			 (loop for a in as
			    do
			      (progn
				(format t "loop :                a ~s~%" a)
				(if alist
				    (loop for e2 in alist
				       do
					 (progn
					   (setf e (append e2 (list a)))
					   (setf rslt-2 (remove e2 rslt :test #'equal))
					   (setf rslt rslt-2)
					   (push e rslt)
					   (format
					    t
					    "loop loop:       e2 ~s, e ~s, rslt ~s~%"
					    e2 e rslt)))
				    (progn
				      (push (list a) rslt)
				      (format t "loop loop:        rslt ~s~%" rslt)))))
			 (helper rslt s3 i)))))

	       (setf angrms (reverse rslt)))))

      (helper angrms p2 0))

    (show-alist angrms)

    angrms))

