(defstruct person
  quickname
  firstname
  lastname)

(defun process-line (line)
  (mapcar (lambda (l)
	    (string-trim " " l))
	  (uiop:split-string line :separator ",")))

(defun string-to-person (str)
  (destructuring-bind (last first) 
      (process-line str)
    (make-person
     :quickname first
     :firstname first
     :lastname last)))

(defun load-people (filepath)
  (with-open-file (f filepath)
    (loop for line = (read-line f nil)
	  while line
	  collect (string-to-person line))))

(defparameter tas-path
  "config/tas")

(defparameter students-path
  "config/students")

(defparameter quicknames-path
  "config/quicknames")

(defparameter tas
  (load-people tas-path))

(defparameter students
  (load-people students-path))

(defparameter colors
  (list
   :red
   :orange
   :blue))

(defun repeat (n x)
  (make-list n :initial-element x))

(defun stretch-colors (colors len)
  (let ((partition-length (ceiling len (length colors))))
    (mapcan (lambda (c) (repeat partition-length c))
	    colors)))

(defparameter color-table
  (mapcar #'cons
	  students
	  (stretch-colors colors (length students))))

(defun color-query (color)
  (remove-if-not (lambda (s) (eql (cdr s) color))
		 color-table))
