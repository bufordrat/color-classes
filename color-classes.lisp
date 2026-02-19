(defstruct ta
  quickname
  firstname
  lastname)

(defun process-line (line)
  (mapcar (lambda (l)
	    (string-trim " " l))
	  (uiop:split-string line :separator ",")))

(defun string-to-ta (str)
  (destructuring-bind (last first) 
      (process-line str)
    (make-ta
     :quickname first
     :firstname first
     :lastname last)))

(defun load-tas (filepath)
  (with-open-file (f filepath)
    (loop for line = (read-line f nil)
	  while line
	  collect (string-to-ta line))))
