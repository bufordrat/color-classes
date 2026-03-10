(require :asdf)
(format *error-output* "~&Working directory: ~A~%" (uiop/os:getcwd))

(load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))
(pushnew (uiop/os:getcwd) asdf:*central-registry*)

(defun build (name)
  (asdf:load-asd (make-pathname :name name :type "asd"))
  (ql:quickload name)
  (sb-ext:save-lisp-and-die
   name
   :toplevel (intern "MAIN" (find-package (intern (string-upcase name))))
   :compression t :save-runtime-options t :executable t))
