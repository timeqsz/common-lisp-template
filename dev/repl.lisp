;;; dev/repl.lisp
;; Usage:
;;   From the project root:
;;     sbcl --load dev/repl.lisp
;;
;; Conventions:
;;   - There is exactly one `.asd` file in the project root.
;;   - The `.asd` filename (without extension) is the ASDF system name.
;;   - The entry package is <SYSTEM-NAME>/MAIN.
;;   - The entry function is MAIN exported from that package.

;;;; 1. Optionally load Quicklisp if available

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;;; 2. Initialize ASDF source-registry for this project

(require :asdf)
(require :uiop)

(let* ((pwd (uiop:getcwd))
       (dir (truename pwd)))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,dir)
     :inherit-configuration)))

;;;; 3. Detect project system name from the single .asd file

(defun detect-project-system ()
  "Detect the unique `.asd` file in the current directory and
   derive its name as the ASDF system name (a string)."
  (let* ((asd-files (uiop:directory-files (uiop:getcwd) "*.asd")))
    (cond
      ((null asd-files)
       (error "No .asd file found in ~A" (uiop:getcwd)))
      ((cdr asd-files)
       (error "Multiple .asd files found in ~A: ~S"
              (uiop:getcwd) asd-files))
      (t
       ;; Extract system name from filename (e.g. foo.asd → "foo")
       (pathname-name (car asd-files))))))

;;;; 4. Main bootstrap logic:
;;;;    - detect system
;;;;    - load it
;;;;    - install (reload) and (run) helpers
;;;;    - print helper info

(let* ((system-name (detect-project-system))
       (entry-package-name (string-upcase (format nil "~A/MAIN" system-name))))

  ;; Load the system
  (asdf:load-system system-name)

  ;; Install a global helper (reload) for reloading.
  ;; NOTE: we define it in the current package (typically CL-USER).
  (setf (symbol-function 'reload)
        (lambda ()
          (asdf:load-system system-name)
          (format t "~&[reload] Reloaded system ~A.~%" system-name)
          system-name))

  ;; Install a global helper (run) = reload + call MAIN.
  (setf (symbol-function 'run)
        (lambda (&rest argv)
          (funcall (symbol-function 'reload))
          (let* ((pkg (or (find-package entry-package-name)
                          (error "Entry package ~A not found."
                                 entry-package-name)))
                 (sym (or (find-symbol "MAIN" pkg)
                          (error "Function MAIN not found in package ~A."
                                 entry-package-name))))
            (apply (symbol-function sym) argv))))

  ;; You stay in CL-USER, and just call (run) / (reload).
  (format t "~&[~A] REPL ready.~%" system-name)
  (format t "  System loaded : ~A~%" system-name)
  (format t "  Entry package : ~A~%" entry-package-name)
  (format t "~&Helpers:~%")
  (format t "  (reload)   ;; Reload the system~%")
  (format t "  (run)  ;; Reload + call main~%"))
