(defmacro const (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

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

(defun student-match (name student)
  (string-equal name (person-quickname student)))

(defun get-student (qname)
  (find-if (lambda (s) (student-match qname s))
	   students))

(defun get-color (qname)
  (find-if (lambda (entry)
	     (if (student-match qname (car entry))
		 (cddr entry)))
	   color-table))

(defun rotate-once (lst)
  (append (last lst) (butlast lst)))

(defun rotate-list (times lst)
  (if (eql times 0)
      lst
      (rotate-once (rotate-list (- times 1) lst))))

(defun pair-up-tas (week tas colors)
  (mapcar #'cons tas (rotate-list week colors)))


