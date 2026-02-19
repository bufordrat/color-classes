(defparameter x 12)

(defstruct ta
  quickname
  firstname
  lastname)

(defun string-to-ta (str)
  (destructuring-bind (last . first) 
      (mapcar (lambda (s) (string-trim " " s))
	      (uiop:split-string str :separator ","))
    (make-ta
     :quickname first
     :firstname first
     :lastname last)))

(defparameter rat
  (with-open-file (f "config/tas")
    (loop for line = (read-line f nil :eof)
	  until (eq line :eof)
	  collect (string-to-ta line))
    (uiop:read-file-string f)))
