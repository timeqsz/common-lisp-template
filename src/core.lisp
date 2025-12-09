;;; src/core.lisp

(uiop:define-package :small-inventory-prototype/core
  (:use :cl)
  ;;  util/logging used for logging messages
  (:import-from :small-inventory-prototype/util/logging
                #:log-info
                #:log-warn)
  (:export
   #:start
   #:stop
   #:do-something))

(in-package :small-inventory-prototype/core)

(defparameter *running* nil
  "Whether small-inventory-prototype is running.")

(defun start ()
  (unless *running*
    (setf *running* t)
    (log-info "small-inventory-prototype started."))
  *running*)

(defun stop ()
  (when *running*
    (setf *running* nil)
    (log-info "small-inventory-prototype stopped."))
  *running*)

(defun do-something (&key (name "world"))
  (unless *running*
    (log-warn "do-something called while not running, starting automatically.")
    (start))
  (log-info "Doing something for ~A" name)
  (format t "Hello, ~A!~%" name)
  name)
