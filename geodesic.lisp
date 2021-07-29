(in-package :geodesic)

;; This is a Common Lisp version of Karney’s 2012 paper:
;; https://doi.org/10.1007/s00190-012-0578-z

(defparameter *a* 6378137 "WGS84 equatorial Earth radius in meter.")
(defparameter *f*
  (/ 1000000000 298257223563)
  #+nil(/ 9241319 2756290147)
  #+nil(/ 311 92758) "WGS84 ellipsoid flattening.")

(defparameter *b* (- *a* (* *f* *a*)))
(defparameter *n* (/ (- *a* *b*) (+ *a* *b*)))

(defparameter *square-e* (/ (- (* *a* *a*) (* *b* *b*)) (* *a* *a*)))
(defparameter *square-e-prime* (/ (- (* *a* *a*) (* *b* *b*)) (* *b* *b*)))

;; equation (6)
(defun reduce-latitude (latitude)
  (atan (* (- 1 *f*) (tan latitude))))

(defun dereduce-latitude (beta)
  (atan (/ (tan beta) (- 1 *f*))))

;; equation (8)
(defun longitude (omega alpha0 i3)
  (- omega (* *f* (sin alpha0) i3)))

;; equation (10)
(defun alpha0 (beta azimuth)
  (atan (* (sin azimuth) (cos beta))
        (abs (complex (cos azimuth) (* (sin azimuth) (sin beta))))))

;; equation (11)
(defun sigma (beta azimuth)
  (if (and (zerop beta)
           (= azimuth (/ pi 2)))
      0
      (atan (sin beta)
            (* (cos azimuth) (cos beta)))))

;; equation (12)
(defun omega (alpha0 sigma)
  (atan (* (sin alpha0) (sin sigma)) (cos sigma)))

;; equation (14) and (13)
(defun alpha-beta (alpha0 sigma)
  (let ((c (complex (* (cos alpha0) (cos sigma)) (sin alpha0))))
    (values (phase c)
            (phase (complex (abs c) (* (cos alpha0) (sin sigma)))))))

;; Note the series expansions is valid to order f^10 and copied from:
;; https://geographiclib.sourceforge.io/html/geodesic.html#geodseries

;; equation (17)
(defun a-1 (epsilon)
  (/ (horner epsilon 49/65536 0 25/16384 0 1/256 0 1/64 0 1/4 0 1)
     (- 1 epsilon)))

;; equation (18)
(defun c-1 (epsilon)
  (list (horner epsilon -3/4096 0 19/2048 0 -1/32 0 3/16 0 -1/2 0)
        (horner epsilon 1/65536 0 7/4096 0 -9/2048 0 1/32 0 -1/16 0 0)
        (horner epsilon 17/24576 0 -3/2048 0 3/256 0 -1/48 0 0 0)
        (horner epsilon 3/8192 0 -11/16384 0 3/512 0 -5/512 0 0 0 0)
        (horner epsilon -3/8192 0 7/2048 0 -7/1280 0 0 0 0 0)
        (horner epsilon -117/524288 0 9/4096 0 -7/2048 0 0 0 0 0 0)
        (horner epsilon 99/65536 0 -33/14336 0 0 0 0 0 0 0)
        (horner epsilon 143/131072 0 -429/262144 0 0 0 0 0 0 0 0)
        (horner epsilon -715/589824 0 0 0 0 0 0 0 0 0)
        (horner epsilon -2431/2621440 0 0 0 0 0 0 0 0 0 0)))

;; equation (15), (23) and (41)
(defun i (sigma an cn)
  (* an (+ sigma (loop for c in cn
                       for i from 1
                       sum (* c (sin (* 2 i sigma)))))))

;; equation (21)
(defun c-prime-1 (epsilon)
  (list (horner epsilon 9039/327680 0 -4879/73728 0 205/1536 0 -9/32 0 1/2 0)
        (horner epsilon 4119073/28311552 0 -86171/368640 0 1335/4096 0 -37/96 0 5/16 0 0)
        (horner epsilon -443327/655360 0 2901/4096 0 -75/128 0 29/96 0 0 0)
        (horner epsilon -2722891/1548288 0 1082857/737280 0 -2391/2560 0 539/1536 0 0 0 0)
        (horner epsilon 1361343/458752 0 -28223/18432 0 3467/7680 0 0 0 0 0)
        (horner epsilon 10820079/1835008 0 -733437/286720 0 38081/61440 0 0 0 0 0 0)
        (horner epsilon -709743/163840 0 459485/516096 0 0 0 0 0 0 0)
        (horner epsilon -550835669/74317824 0 109167851/82575360 0 0 0 0 0 0 0 0)
        (horner epsilon 83141299/41287680 0 0 0 0 0 0 0 0 0)
        (horner epsilon 9303339907/2972712960 0 0 0 0 0 0 0 0 0 0)))

;; equation (24) and (25)
(defparameter *F-a-3*
  (list 245/32768
        (horner *n* 35/4096 175/16384)
        (horner *n* 21/2048 35/2048 25/2048)
        (horner *n* 7/512 35/1024 15/1024 5/256)
        (horner *n* 7/256 35/256 5/256 5/128 3/128)
        (horner *n* -35/128 5/128 5/32 1/32 3/64)
        (horner *n* -5/16 1/16 3/16 1/16)
        (horner *n* -3/8 1/8 1/4)
        (horner *n* -1/2 1/2)
        0)
  "Factors for A-3.")

(defparameter *P-c-3*
  (list (list 345/32768
              (horner *n* 109/16384 435/32768)
              (horner *n* 83/16384 189/16384 243/16384)
              (horner *n* 1/512 13/1024 5/512 21/1024)
              (horner *n* -7/512 1/256 3/512 11/512 3/128)
              (horner *n* -7/128 -1/64 1/64 1/64 5/128)
              (horner *n* -5/64 -1/64 3/64 3/64)
              (horner *n* -1/8 0 1/8)
              (horner *n* -1/4 1/4)
              0)
        (list 255/32768
              (horner *n* 47/8192 287/32768)
              (horner *n* 31/16384 39/8192 187/16384)
              (horner *n* -47/4096 -39/8192 69/8192 27/2048)
              (horner *n* -3/256 -7/256 -1/128 1/256 5/256)
              (horner *n* 7/256 -3/128 -9/256 1/128 3/128)
              (horner *n* 1/32 -3/64 -1/32 3/64)
              (horner *n* 1/32 -3/32 1/16)
              0 0)
        (list 581/98304
              (horner *n* 95/49152 243/32768)
              (horner *n* -383/49152 143/49152 139/16384)
              (horner *n* -47/3072 -71/6144 -1/1024 3/256)
              (horner *n* 65/3072 5/3072 -77/3072 -1/384 7/512)
              (horner *n* -1/128 5/192 -1/64 -5/192 3/128)
              (horner *n* -1/192 5/192 -3/64 5/192)
              0 0 0)
        (list 171/32768
              (horner *n* 3/8192 193/32768)
              (horner *n* -165/16384 -23/8192 127/16384)
              (horner *n* 39/4096 -129/8192 -43/8192 9/1024)
              (horner *n* -21/2048 9/512 -7/2048 -5/256 7/512)
              (horner *n* 1/1024 -7/1024 5/256 -7/256 7/512)
              0 0 0 0)
        (list 141/32768
              (horner *n* -55/16384 179/32768)
              (horner *n* -781/81920 -91/16384 99/16384)
              (horner *n* 57/5120 3/2048 -15/1024 9/1024)
              (horner *n* 9/5120 -7/1024 15/1024 -9/512 21/2560)
              0 0 0 0 0)
        (list 33/8192
              (horner *n* -253/49152 143/32768)
              (horner *n* 55/16384 -275/24576 99/16384)
              (horner *n* -77/12288 275/24576 -99/8192 11/2048)
              0 0 0 0 0 0)
        (list 429/131072
              (horner *n* -143/16384 143/32768)
              (horner *n* 143/16384 -143/16384 429/114688)
              0 0 0 0 0 0 0)
        (list 429/131072
              (horner *n* -429/65536 715/262144)
              0 0 0 0 0 0 0 0)
        (list 2431/1179648 0 0 0 0 0 0 0 0 0))
  "Polynomial factors for C-3.")

(defun ac-3 (epsilon)
  (values
   (- 1 (apply #'horner epsilon *F-a-3*))
   (loop for p in *P-c-3*
         collect (apply #'horner epsilon p))))

;; equation (20)
(defun sigma-tau (tau c-prime-1)
  (+ tau (loop for c in c-prime-1
               for i from 1
               sum (* c (sin (* 2 i tau))))))

(defun direct (latitude azimuth distance)
  "LATITUDE and AZIMUTH in radians. DISTANCE in meters."
  (let* ((beta1 (reduce-latitude latitude))
         (alpha0 (alpha0 beta1 azimuth))
         (sigma1 (sigma beta1 azimuth))
         (omega1 (omega alpha0 sigma1))
         (square-k (* *square-e-prime* (cos alpha0) (cos alpha0)))
         (epsilon (let ((c (sqrt (1+ square-k)))) (/ (1- c) (1+ c))))
         (a1 (a-1 epsilon))
         (i1 (i sigma1 a1 (c-1 epsilon)))
         (s1 (* i1 *b*))
         (s2 (+ s1 distance))
         (tau2 (/ s2 (* *b* a1)))
         (sigma2 (sigma-tau tau2 (c-prime-1 epsilon))))
    (multiple-value-bind (alpha2 beta2) (alpha-beta alpha0 sigma2)
      (multiple-value-bind (a3 c3) (ac-3 epsilon)
        (let* ((omega2 (omega alpha0 sigma2))
               (i3-sigma1 (i sigma1 a3 c3))
               (i3-sigma2 (i sigma2 a3 c3))
               (lon12 (- (longitude omega2 alpha0 i3-sigma2)
                         (longitude omega1 alpha0 i3-sigma1))))
          (values (dereduce-latitude beta2) lon12 alpha2))))))

;; (direct (radians 40) (radians 30) 10e6)

;; equation (40)
(defun j (sigma epsilon)
  (- (i sigma (a-1 epsilon) (c-1 epsilon))
     (i sigma (a-2 epsilon) (c-2 epsilon))))

;; equation (38)
(defun m12 (k2 sigma1 sigma2 epsilon)
  (let ((cs1 (cos sigma1))
        (ss1 (sin sigma1))
        (cs2 (cos sigma2))
        (ss2 (sin sigma2)))
    (* *b*
       (- (* (sqrt (1+ (* k2 ss2 ss2))) cs1 ss2)
          (* (sqrt (1+ (* k2 ss1 ss1))) ss1 cs2)
          (* cs1 cs2 (- (j sigma2 epsilon) (j sigma1 epsilon)))))))

;; equation (42)
(defun a-2 (epsilon)
  (/ (horner epsilon -931/65536 0 -375/16384 0 -11/256 0 -7/64 0 -3/4 0 1)
     (1+ epsilon)))

;; equation (43)
(defun c-2 (epsilon)
  (list (horner epsilon 59/4096 0 41/2048 0 1/32 0 1/16 0 1/2 0)
        (horner epsilon 557/65536 0 47/4096 0 35/2048 0 1/32 0 3/16 0 0)
        (horner epsilon 191/24576 0 23/2048 0 5/256 0 5/48 0 0 0)
        (horner epsilon 47/8192 0 133/16384 0 7/512 0 35/512 0 0 0 0)
        (horner epsilon 51/8192 0 21/2048 0 63/1280 0 0 0 0 0)
        (horner epsilon 2607/524288 0 33/4096 0 77/2048 0 0 0 0 0 0)
        (horner epsilon 429/65536 0 429/14336 0 0 0 0 0 0 0)
        (horner epsilon 715/131072 0 6435/262144 0 0 0 0 0 0 0 0)
        (horner epsilon 12155/589824 0 0 0 0 0 0 0 0 0)
        (horner epsilon 46189/262144 0 0 0 0 0 0 0 0 0 0)))

;; equation (44)
(defun normalize-latitudes (lat1 lat2)
  "lat1 <= 0 and lat1 <= lat2 <= -lat1 (i.e. lat2 is closest to zero)."
  ;; permute if lat1 is closest to zero
  (when (< (abs lat1) (abs lat2))
    (rotatef lat1 lat2))
  (values (* (- (signum lat1)) lat1)
          (* (- (signum lat1)) lat2)))

;; equation (5) and (45)
(defun alpha2 (alpha1 beta1 beta2)
  (let ((ca1 (cos alpha1))
        (sa1 (sin alpha1))
        (cb1 (cos beta1))
        (cb2 (cos beta2)))
    (atan (/ (* sa1 cb1) cb2)
          (/ (sqrt (+ (* ca1 ca1 cb1 cb1) (- (* cb2 cb2) (* cb1 cb1)))) cb2))))

;; equation (48)
(defun omega-bar (beta1 beta2)
  (sqrt (- 1 (* *square-e* (expt (/ (+ (cos beta1) (cos beta2)) 2) 2)))))

;; equation (49) and (50)
(defun z1 (beta1 beta2 omega12)
  (complex (- (* (cos beta1) (sin beta2))
              (* (sin beta1) (cos beta2) (cos omega12)))
           (* (cos beta2) (sin omega12))))

(defun z2 (beta1 beta2 omega12)
  (complex (- (* (cos beta1) (sin beta2) (cos omega12))
              (* (sin beta1) (cos beta2)))
           (* (cos beta1) (sin omega12))))

;; equation (51)
(defun sigma12 (beta1 beta2 omega12 z1)
  (phase (complex (+ (* (sin beta1) (sin beta2))
                     (* (cos beta1) (cos beta2) (cos omega12)))
                  (abs z1))))

(defun nearly-antipodal-p (lat1 lat2 lon12)
  (let ((epsilon (/ pi 360)))
    (or (and (< (abs (+ lat1 lat2)) epsilon)
             (< (abs (- lon12 pi)) epsilon))
        (> lon12 (- pi epsilon)))))

(defun init-alpha1 (lat1 lat2 lon12 beta1 beta2 omega12)
  (labels ((myplusp (x)
             (plusp (realpart x))))
    (if (nearly-antipodal-p lat1 lat2 lon12)
        (let* ((delta (* *f* *a* pi (cos beta1) (cos beta1)))
               ;; equation (53)
               (x (/ (* (- lon12 pi) *a* (cos beta1)) delta))
               (y (/ (* (+ beta1 beta2) *a*) delta))
               (y2 (* y y))
               (roots (quartic-roots 1 2 (- 1 (* x x) y2) (* -2 y2) (- y2)))
               (mu (realpart (find-if #'myplusp roots))))
          (if (zerop y)
              (atan (- x) (* (signum x) (sqrt (- 1 (* x x)))))
              (atan (- (/ x (1+ mu))) (/ y mu))))
        (phase (z1 beta1 beta2 omega12)))))

;; equation (46)
(defun delta-alpha1 (delta-l12 m12 alpha2 beta2)
  (- (/ (* *a* delta-l12 (cos alpha2) (cos beta2))
        m12)))

(defun solve-triangle (alpha1 beta1 beta2)
  (let* ((alpha0 (alpha0 beta1 alpha1))
         (sigma1 (sigma beta1 alpha1))
         (omega1 (omega alpha0 sigma1))
         (alpha2 (alpha2 alpha1 beta1 beta2))
         (sigma2 (sigma beta2 alpha2))
         (omega2 (omega alpha0 sigma2))
         ;; equation (9)
         (k (* (sqrt *square-e-prime*) (cos alpha0)))
         (k2 (* k k))
         (epsilon (/ (1- (sqrt (1+ k2)))
                     (1+ (sqrt (1+ k2))))))
    (values alpha0 sigma1 omega1 alpha2 sigma2 omega2 k2 epsilon)))

(defun inverse (lat1 lat2 lon12)
  (multiple-value-bind (lat1 lat2) (normalize-latitudes lat1 lat2)
    (let* ((beta1 (reduce-latitude lat1))
           (beta2 (reduce-latitude lat2))
           (omega-bar (omega-bar beta1 beta2))
           (omega12 (/ lon12 omega-bar))
           (alpha1 (init-alpha1 lat1 lat2 lon12 beta1 beta2 omega12))
           (delta-lon12 (/ pi 180))
           (tolerance (if (> lon12 (radians 179.5))
                          (/ pi 1d15)
                          (/ pi 1d14))))
      ;; First: find correct alpha1
      (loop repeat 30
            while (> (abs delta-lon12) tolerance)
            do (multiple-value-bind (alpha0 sigma1 omega1 alpha2 sigma2 omega2 k2 epsilon)
                   (solve-triangle alpha1 beta1 beta2)
                 (multiple-value-bind (a3 c3) (ac-3 epsilon)
                   (let* ((i3-sigma1 (i sigma1 a3 c3))
                          (i3-sigma2 (i sigma2 a3 c3))
                          (lon12-new (abs (- (longitude omega2 alpha0 i3-sigma2)
                                             (longitude omega1 alpha0 i3-sigma1))))
                          (m12 (m12 k2 sigma1 sigma2 epsilon)))
                     (setf delta-lon12 (- lon12-new lon12)
                           alpha1 (normalize (+ alpha1 (delta-alpha1 delta-lon12 m12 alpha2 beta2))))))))
      ;; Second: resolve s12
      (multiple-value-bind (alpha0 sigma1 omega1 alpha2 sigma2 omega2 k2 epsilon)
          (solve-triangle alpha1 beta1 beta2)
        (declare (ignore alpha0 omega1 omega2 k2))
        ;; equation (7)
        (let ((s1 (* *b* (i sigma1 (a-1 epsilon) (c-1 epsilon))))
              (s2 (* *b* (i sigma2 (a-1 epsilon) (c-1 epsilon)))))
          (values (abs (- s2 s1)) alpha1 alpha2))))))

;; (inverse (radians -30.12345d0) (radians -30.12344d0) (radians 0.00005d0))
;; (inverse (radians -30d0) (radians 29.9d0) (radians 179.8d0))
;; (inverse (radians 1.8363687) (radians -2.3656454) (radians 179.76564))
;; (inverse (radians (parse-float "1.836368651078")) (radians (parse-float "-2.365645296794186765")) (radians (parse-float "179.765637637545230805")))
