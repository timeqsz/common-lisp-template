;;; src/core.lisp

(uiop:define-package :common-lisp-template/core
  (:use :cl)
  ;;  util/logging used for logging messages
  (:import-from :common-lisp-template/util/logging
                #:log-info
                #:log-warn)
  (:export
   #:start
   #:stop
   #:do-something))

(in-package :common-lisp-template/core)

(defparameter *running* nil
  "Whether common-lisp-template is running.")

(defun start ()
  (unless *running*
    (setf *running* t)
    (log-info "common-lisp-template started."))
  *running*)

(defun stop ()
  (when *running*
    (setf *running* nil)
    (log-info "common-lisp-template stopped."))
  *running*)

(defun do-something (&key (name "world"))
  (unless *running*
    (log-warn "do-something called while not running, starting automatically.")
    (start))
  (log-info "Doing something for ~A" name)
  (format t "Hello, ~A!~%" name)
  name)
