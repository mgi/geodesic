(in-package :geodesic)

(defun plusp-and-value (string)
  (let ((minus-pos (position #\- string)))
    (if minus-pos
        (values nil (subseq string (1+ minus-pos)))
        (values t string))))

(defun parse-float (string)
  "Parse float STRING into an exact rational. XXX it is really simple
and the form should be \"{-}[0-9]+\" or \"{-}[0-9]*.[0-9]+\""
  (multiple-value-bind (plusp value-string) (plusp-and-value string)
    (let ((dot-pos (position #\. value-string)))
      (* (if plusp 1 -1)
         (if (null dot-pos)
             (parse-integer value-string)
             (let* ((integer (if (zerop dot-pos)
                                 0
                                 (parse-integer value-string :end dot-pos)))
                    (decimal (parse-integer value-string :start (1+ dot-pos)))
                    (n-decimal (- (length value-string) (1+ dot-pos)))
                    (denominator (expt 10 n-decimal))
                    (numerator (* integer denominator)))
               (/ (+ numerator decimal) denominator)))))))

(defun radians (angle)
  (* (/ pi 180) angle))

(defun degrees (angle)
  (/ angle (/ pi 180)))

(defun normalize (angle)
  "Normalize angle between -π and π."
  (let ((2pi (* 2 pi)))
    (multiple-value-bind (div angle) (truncate angle 2pi)
      (declare (ignore div))
      (loop until (<= angle pi)
            do (decf angle 2pi))
      (loop until (>= angle (- pi))
            do (incf angle 2pi))
      angle)))

;; Aliases to use polygon:point as geographical one.
;;
;; Latitude and longitude are in degrees. Altitude
;; is in meters.
(setf (fdefinition 'point-longitude) #'polygon:point-x
      (fdefinition 'point-latitude) #'polygon:point-y
      (fdefinition 'point-altitude) #'polygon:point-z)

(defun make-geo-point (latitude longitude &optional (altitude 0))
  "LATITUDE and LONGITUDE should be in decimal degrees. ALTITUDE should
be in meters."
  (polygon:make-point :x longitude :y latitude :z altitude))
