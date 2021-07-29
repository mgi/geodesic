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
               (declare (ignore lon1 lon2 a12 m12 surf12))
               (multiple-value-bind (mylat2 d myazi2)
                   (direct (radians lat1) (radians azi1) s12)
                 (declare (ignore d))
                 (is (about= mylat2 (radians lat2) 1d-14))
                 (is (about= myazi2 (radians azi2) 1d-10)))))))

(defun inverse-test (filename)
  (with-open-file (fd filename)
    (loop with failed = 0
          for line = (read-line fd nil)
          for line-count from 1
          while line
          do (destructuring-bind (lat1 lon1 azi1 lat2 lon2 azi2 s12 a12 m12 surf12)
                 (parse-line line)
               (declare (ignore azi1 azi2 a12 m12 surf12))
               (multiple-value-bind (mys12 myazi1 myazi2)
                   (inverse (radians lat1) (radians lat2) (radians (- lon2 lon1)))
                 (declare (ignore myazi1 myazi2))
                 (is (about= mys12 s12 1/1000))
                 (unless (about= mys12 s12 1/1000)
                   (incf failed)
                   #+nil
                   (format t "~&~a: ~@{~a ~}~%"
                           line-count
                           (float lat1 1d0) (float lat2 1d0) (float (- lon2 lon1) 1d0)
                           (float s12 1d0) mys12
                           (- s12 mys12)))))
          finally (unless (zerop failed)
                    (format t "~&~d failed on ~d~%" failed (1- line-count))))))

(test geod-short-direct (direct-test "GeodTest-short.dat"))
(test geod-short-inverse (inverse-test "GeodTest-short.dat"))

;; Only if you have time on your hands (and not everything pass :-|)
#+nil
(test geod-long-direct (direct-test "GeodTest.dat"))
#+nil
(test geod-long-inverse (inverse-test "GeodTest.dat"))
