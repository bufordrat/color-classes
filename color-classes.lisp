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

(defun load-tas (filepath)
  (with-open-file (f filepath)
    (loop for line = (read-line f nil :eof)
	  until (eq line :eof)
	  collect (string-to-ta line))))
