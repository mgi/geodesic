(defpackage :geodesic/test
  (:use :common-lisp :geodesic :1am :computable-reals))

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

(defun rat-approx-r (x k)
  (/ (approx-r x k) (expt 2 k)))

(test geod-test-short
  (with-open-file (fd "GeodTest-short.dat")
    (loop with epsilon = 1e-12
          for line = (read-line fd nil)
          while line
          do (destructuring-bind (lat1 lon1 azi1 lat2 lon2 azi2 s12 a12 m12 surf12) (parse-line line)
               (declare (ignore a12 m12 surf12))
               (multiple-value-bind (mylat2 d myazi2) (direct (radians lat1) (radians azi1) s12)
                 (declare (ignore d))
                 (is (about= (rat-approx-r (degrees mylat2) 40) lat2 epsilon))
                 (is (about= (rat-approx-r (degrees myazi2) 40) azi2 epsilon)))))))
