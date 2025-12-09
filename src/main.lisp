;;; src/main.lisp

(uiop:define-package :small-inventory-prototype/main
  (:use :cl)
  (:import-from :small-inventory-prototype/core
                #:start
                #:stop
                #:do-something)
  (:export
   #:main))

(in-package :small-inventory-prototype/main)

(defun main (&rest argv)
  "Entry point for small-inventory-prototype demo.
   Can be called directly in the REPL or made into an executable wrapper."
  (declare (ignore argv))
  (start)
  (unwind-protect
       (progn
         (format t "Running small-inventory-prototype demo...~%")
         (format t "~{~A~^ ~}~%" argv))
         (do-something :name "world"))
    (stop))
