(defparameter x 12)

(defstruct ta
  quick-name
  first-name
  last-name)

(defun string-to-ta (str)
  (uiop:split-string str :separator ", "))

(defparameter rat
  (with-open-file (f "config/tas")
    (uiop:read-file-string f)))

