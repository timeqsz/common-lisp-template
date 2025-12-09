;;; src/main.lisp

(uiop:define-package :common-lisp-template/main
  (:use :cl)
  (:import-from :common-lisp-template/core
                #:start
                #:stop
                #:do-something)
  (:export
   #:main))

(in-package :common-lisp-template/main)

(defun main (&rest argv)
  "Entry point for common-lisp-template demo.
   Can be called directly in the REPL or made into an executable wrapper."
  (declare (ignore argv))
  (start)
  (unwind-protect
       (progn
         (format t "Running common-lisp-template demo...~%")
         (format t "~{~A~^ ~}~%" argv))
         (do-something :name "world"))
    (stop))
