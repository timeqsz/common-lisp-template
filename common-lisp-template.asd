;;; template.asd --- A Common Lisp project template using package-inferred-system

(asdf:defsystem "common-lisp-template"
  :description "A generic Common Lisp project template using package-inferred-system."
  :author "Sylvan Lv"
  :license "MIT"
  :version "0.1.0"

  ;; use package-inferred-system extended from asdf-package-system
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system

  ;; source 
  :pathname "src"

  ;; entry point
  :depends-on ("common-lisp-template/main"))