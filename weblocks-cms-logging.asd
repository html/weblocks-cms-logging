;;;; weblocks-cms-logging.asd

(asdf:defsystem #:weblocks-cms-logging
  :serial t
  :version (:read-from-file "version.lisp-expr")
  :description "Describe weblocks-cms-logging here"
  :author "Olexiy Zamkoviy"
  :license "Specify license here"
  :depends-on (#:weblocks-cms #:yaclml #:weblocks-filtering-widget #:weblocks-utils #:group-by)
  :components ((:file "package")
               (:file "weblocks-cms-logging")))

