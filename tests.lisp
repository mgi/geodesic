(defpackage :geodesic/test
  (:use :common-lisp :geodesic :1am))

(in-package :geodesic/test)

(declaim (optimize (debug 3)))

;; Redefine 1am::passed with less dot.
(defun 1am::passed ()
  (when (zerop (mod 1am::*pass-count* 100))
    (write-char #\.))
  (when 1am::*pass-count*
    (incf 1am::*pass-count*))
  (values))

(defvar *short-file*
  (merge-pathnames "GeodTest-short.dat"
                   (load-time-value (or #.*compile-file-pathname* *load-pathname*))))

(defun about= (a b epsilon)
  (< (abs (- b a)) epsilon))

(defun parse-line (line)
  (mapcar #'parse-float (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))

;; test with examples from https://dlmf.nist.gov/1.11 and
;; https://www.1728.org/cubic2.htm
(test roots
  (let ((epsilon 1e-6))
    (destructuring-bind (r1 r2 r3) (cubic-roots 1 -6 6 -2)
      (let* ((rho (exp (complex 0 (* 2/3 pi))))
             (rho2 (* rho rho))
             (3rt2 (expt 2 (/ 3)))
             (3rt4 (expt 4 (/ 3))))
        (is (about= r1 (+ 2 3rt4 3rt2) epsilon))
        (is (about= r2 (+ 2 (* 3rt4 rho) (* 3rt2 rho2)) epsilon))
        (is (about= r3 (+ 2 (* 3rt4 rho2) (* 3rt2 rho)) epsilon))))
    (destructuring-bind (r1 r2 r3) (cubic-roots 2 -4 -22 24)
      (is (about= r1 4 epsilon))
      (is (about= r2 -3 epsilon))
      (is (about= r3 1 epsilon)))
    (destructuring-bind (r1 r2 r3) (cubic-roots 3 -10 14 27)
      (is (= (round r1) -1))
      (is (about= r2 (complex 2.166668 2.074981) epsilon))
      (is (about= r3 (complex 2.166668 -2.074981) epsilon)))
    (destructuring-bind (r1 r2 r3) (cubic-roots 1 6 12 8)
      (is (about= r1 -2 epsilon))
      (is (about= r2 -2 epsilon))
      (is (about= r3 -2 epsilon)))
    (destructuring-bind (r1 r2 r3 r4) (quartic-roots 1 -4 0 5 2)
      (is (about= r1 (/ (+ 3 (sqrt 17)) 2) epsilon))
      (is (about= r2 (/ (+ 1 (sqrt 5)) 2) epsilon))
      (is (about= r3 (/ (- 1 (sqrt 5)) 2) epsilon))
      (is (about= r4 (/ (- 3 (sqrt 17)) 2) epsilon)))))

(defun direct-test (filename)
  (with-open-file (fd filename)
    (loop for line = (read-line fd nil)
          while line
          do (destructuring-bind (lat1 lon1 azi1 lat2 lon2 azi2 s12 a12 m12 surf12)
                 (parse-line line)
               (declare (ignore a12 m12 surf12))
               (multiple-value-bind (mylat2 mylon12 myazi2)
                   (direct (radians lat1) (radians azi1) s12)
                 (is (about= mylat2 (radians lat2) 1d-14))
                 (is (about= mylon12 (radians (- lon2 lon1)) 1d-8))
                 (is (about= myazi2 (radians azi2) 1d-10)))))))

(defun inverse-test (filename)
  (with-open-file (fd filename)
    (loop for line = (read-line fd nil)
          while line
          do (destructuring-bind (lat1 lon1 azi1 lat2 lon2 azi2 s12 a12 m12 surf12)
                 (parse-line line)
               (declare (ignore a12 m12 surf12))
               ;; we have less precision on antipodal points
               (let ((azimuth-epsilon (if (> (- lon2 lon1) 179) 1d-4 1d-7)))
                 (multiple-value-bind (mys12 myazi1 myazi2)
                     (inverse (radians lat1) (radians lat2) (radians (- lon2 lon1)))
                   (is (about= mys12 s12 1/1000))
                   (is (about= myazi1 (radians azi1) azimuth-epsilon))
                   (is (about= myazi2 (radians azi2) azimuth-epsilon))))))))

(test geod-short-direct (direct-test *short-file*))
(test geod-short-inverse (inverse-test *short-file*))

;; Only if you have time on your hands (and last time I checked, not
;; everything pass :-|)
#+nil
(test geod-long-direct (direct-test "GeodTest.dat"))
#+nil
(test geod-long-inverse (inverse-test "GeodTest.dat"))
