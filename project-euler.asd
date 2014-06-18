(in-package :cl-user)

(defpackage :project-euler-asd
  (:use :cl :asdf))

(in-package :project-euler-asd)

(defsystem "project-euler"
  :components ((:file "solutions"
                      :depends-on ("utilities"))
               (:file "utilities")))
