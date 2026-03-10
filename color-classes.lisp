(defpackage :cc-lib
  (:use :cl)
  (:export :main))

(in-package :cc-lib)

(require 'uiop)

(defstruct person
  quickname
  firstname
  lastname)

(defun process-line (line)
  (mapcar (lambda (l) (string-trim " " l))
	  (uiop:split-string
	   line
	   :separator ",")))

(defun string-to-person (str)
  (destructuring-bind (last first) (process-line str)
    (flet ((shorten-firstname (firstname)
	     (car (uiop:split-string
		   firstname
		   :separator " "))))
      (make-person
       :quickname (shorten-firstname first)
       :firstname first
       :lastname last))))

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
  '((:blue "#2a71e6" "#e8f4ff")
    (:orange "#cc5700" "#fff7f1")
    (:red "#a61c00" "#ffeded")))

(defparameter colors
  (mapcar #'car color-codes))

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

(defun rotate-list (times lst)
  (flet ((rotate-once (lst)
	   (append (cdr lst) (list (car lst)))))
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
	   (ta (color-to-ta assignment color))
	   (color-string (string color)))
      (values ta color-string))))

(defparameter first-assignment 1)
(defparameter final-assignment 8)

(defun get-columns (cs)
  (loop for i
	from (- first-assignment 1) upto (- final-assignment 1)
	collect (rotate-list i cs)))

(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun color-to-string (color)
  (let* ((row (cdr (assoc color color-codes)))
	 (fg (first row))
	 (bg (second row)))
    (if color
	(concatenate 'string
		     " style=\"text-align:center;"
		     "color:"
		     fg
		     ";background-color:"
		     bg
		     ";\"")
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
  (funcall (tag "table") content))

(defun concat (list-of-strings)
  (apply #'concatenate 'string list-of-strings))

(defun each-html-row (row)
  (tr (apply #'concatenate 'string
	     (mapcar
	      (lambda (color) (td (string-downcase (string color)) color))
	      row))))

(defun columns-to-html-table (columns)
  (let ((rows (transpose columns)))
    (apply #'concatenate 'string (mapcar #'each-html-row rows))))

(defun colors-to-html (colors)
  (html (body (table (columns-to-html-table (get-columns colors))))))

(defun write-html-file (colors)
  (with-open-file (str "test.html"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str (colors-to-html colors))))

(defun main ()
  (let* ((assignment (parse-integer (second sb-ext:*posix-argv*)))
	 (student (third sb-ext:*posix-argv*))
	 (outputs (multiple-value-list
		   (student-to-ta assignment student))))
    ;; (mapc #'write-line outputs)
    (format t "~{~A~%~}" outputs)))

;; 3 --student raj
;;  : print :
;;  Jyotsna
;;  BLUE

;; 7 --ta joytsna
;;  : print :
;;  BLUE

;; 1 --color blue
;;  : print :
;;  Jonathan
