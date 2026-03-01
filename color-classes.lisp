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

(defparameter color-codes
  (list '(:red . ())))

(defparameter colors
  (list
   :red
   :orange
   :blue))

(defun stretch-colors (colors len)
  (flet ((repeat (n x)
	   (make-list n :initial-element x)))
    (let ((partition-length (ceiling len (length colors))))
      (mapcan (lambda (c)
		(repeat partition-length c))
	      colors))))

(defparameter color-table
  (mapcar #'cons
	  students
	  (stretch-colors colors (length students))))

(defun student-match (name student)
  (string-equal name (person-quickname student)))

(defun get-student (qname)
  (find-if (lambda (s) (student-match qname s))
	   students))

(defun rotate-list (times lst)
  (flet ((rotate-once (lst)
	   (append (last lst) (butlast lst))))
    (if (eql times 0)
	lst
	(rotate-once (rotate-list (- times 1) lst)))))

(defun pair-up-tas (assignment tas colors)
  (mapcar #'cons tas
	  (rotate-list (- assignment 1) colors)))

(defun color-to-ta (assignment color)
  (person-quickname
   (car
    (rassoc color (pair-up-tas assignment tas colors)))))

(defun assignment-to-week (assignment)
  (mapcar (lambda (x)
	    (cons (person-quickname (car x)) (cdr x)))
	  (pair-up-tas assignment tas colors)))

(defun ta-to-color (assignment ta)
  (let* ((week (assignment-to-week assignment))
	 (pair (assoc ta week :test #'string-equal)))
    (cdr pair)))

(defun student-to-ta (assignment qname)
  (flet ((get-color (qname)
	   (cdr
	    (find-if
	     (lambda (entry)
	       (student-match qname (car entry)))
	     color-table))))
    (let* ((color (get-color qname))
	   (ta (color-to-ta assignment color)))
      ta)))

(defun get-rows-full (ts cs)
  (flet ((each-row (row)
	   (mapcar #'cons ts row)))
    (let ((weeks
	    (loop for i
		  from 1 upto 9
		  collect (rotate-list i cs))))
      (mapcar #'each-row weeks))))

(defun get-rows (ts cs)
  (let ((quicknames (mapcar #'person-quickname ts)))
    (get-rows-full quicknames cs)))

(defun create-inner-columns (number-of-assignments colors)
  (flet ((each-row (i)
	   (cons i (rotate-list i colors))))
    (loop for i
	  from 1 upto number-of-assignments
	  collect (each-row i))))

(defun color-to-string (color)
  (let* ((color-string
	   (string-downcase (string color))))
    (if color
	(concatenate 'string " style=\"color:" color-string ";\"")
	"")))
	 
(defun tag (tagname &optional color)
    (lambda (content)
      (concatenate 'string
		   "<" tagname (color-to-string color) ">"
		   content
		   "</" tagname ">")))

(defun html (content)
  (funcall (tag "html") content))

(defun body (content)
  (funcall (tag "body") content))

(defun tr (content)
  (funcall (tag "tr") content))

(defun td (content &optional color)
    (funcall (tag "td" color) content))

(defun table (content)
  (funcall (tag "tr") content))
