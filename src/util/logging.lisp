;;; src/util/logging.lisp

(uiop:define-package :small-inventory-prototype/util/logging
  (:use :cl)
  (:export
   #:log-info
   #:log-warn
   #:log-error))

(in-package :small-inventory-prototype/util/logging)

(defun %timestamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun log-info (fmt &rest args)
  (apply #'format *standard-output*
         (concatenate 'string "[" (%timestamp) "] [INFO] " fmt "~%")
         args))

(defun log-warn (fmt &rest args)
  (apply #'format *standard-output*
         (concatenate 'string "[" (%timestamp) "] [WARN] " fmt "~%")
         args))

(defun log-error (fmt &rest args)
  (apply #'format *standard-output*
         (concatenate 'string "[" (%timestamp) "] [ERROR] " fmt "~%")
         args))
