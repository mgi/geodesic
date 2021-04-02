(in-package :asdf-user)

(asdf:defsystem :geodesic
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Library for geodesic calculations."
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "geodesic"))
  :in-order-to ((test-op (test-op :geodesic/test))))

(asdf:defsystem :geodesic/test
  :serial t
  :depends-on (:1am :parse-number :split-sequence :geodesic)
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :1am '#:run)))
