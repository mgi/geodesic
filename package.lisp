(defpackage :geodesic
  (:use :common-lisp)
  (:export #:parse-float
           #:radians
           #:degrees
           #:cubic-roots
           #:quartic-roots
           #:make-geo-point
           #:point-latitude
           #:point-longitude
           #:point-altitude
           #:direct
           #:indirect
           #:move))
