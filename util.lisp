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

(defun abs-r (realpart imagpart)
  (sqrt-r (+r (*r realpart realpart)
              (*r imagpart imagpart))))

(defun radians (angle)
  (*r +pi-r+ 1/180 angle))

(defun degrees (angle)
  (*r 180 (/r +pi-r+) angle))
