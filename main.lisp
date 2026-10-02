(in-package :color-classes)

(defun main ()
  (let* ((assignment (parse-integer (second sb-ext:*posix-argv*)))
	 (student (third sb-ext:*posix-argv*))
	 (outputs (multiple-value-list
		   (student-to-ta assignment student))))
    (format t "~{~A~%~}" outputs)))
