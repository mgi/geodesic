(defpackage :geodesic/test
  (:use :common-lisp :geodesic :1am))

(in-package :geodesic/test)

;; Redefine 1am::passed with less dot.
(defun 1am::passed ()
  (when (zerop (mod 1am::*pass-count* 100))
    (write-char #\.))
  (when 1am::*pass-count*
    (incf 1am::*pass-count*))
  (values))

(defun about= (a b epsilon)
  (< (abs (- b a)) epsilon))

(defun parse-line (line)
  (mapcar #'parse-float (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))

(test geod-test-short
  (with-open-file (fd "GeodTest-short.dat")
    (loop for line = (read-line fd nil)
          while line
          do (destructuring-bind (lat1 lon1 azi1 lat2 lon2 azi2 s12 a12 m12 surf12) (parse-line line)
               (declare (ignore a12 m12 surf12))
               (multiple-value-bind (mylat2 d myazi2) (direct (radians lat1) (radians azi1) s12)
                 (declare (ignore d))
                 (is (about= mylat2 (radians lat2) 1e-14))
                 (is (about= myazi2 (radians azi2) 1e-10)))))))
