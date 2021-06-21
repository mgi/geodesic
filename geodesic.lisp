(in-package :geodesic)

;; This is a Common Lisp version of Karney’s 2012 paper:
;; https://doi.org/10.1007/s00190-012-0578-z

(defparameter *a* 6378137 "WGS84 equatorial Earth radius in meter.")
(defparameter *f* (/ 9241319 2756290147) #+nil(/ 311 92758) "WGS84 ellipsoid flattening.")

(defparameter *b* (- *a* (* *f* *a*)))
(defparameter *n* (/ (- *a* *b*) (+ *a* *b*)))

(let ((square-a (* *a* *a*))
      (square-b (* *b* *b*)))
  (defparameter *square-e* (/ (- square-a square-b) square-a))
  (defparameter *square-e-prime* (/ (- square-a square-b) square-b)))

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
  (phase (complex (abs (complex (cos azimuth) (* (sin azimuth) (sin beta))))
                  (* (sin azimuth) (cos beta)))))

;; equation (11)
(defun sigma (beta azimuth)
  (if (and (zerop beta)
           (= azimuth (/ pi 2)))
      0
      (phase (complex (* (cos azimuth) (cos beta))
                      (sin beta)))))

;; equation (12)
(defun omega (alpha0 sigma)
  (phase (complex (cos sigma) (* (sin alpha0) (sin sigma)))))

;; equation (14) and (13)
(defun alpha-beta (alpha0 sigma)
  (let ((c (complex (* (cos alpha0) (cos sigma)) (sin alpha0))))
    (values (phase c)
            (phase (complex (abs c) (* (cos alpha0) (sin sigma)))))))

;; Note the series expansions is valid to order f^10 and copied from:
;; https://geographiclib.sourceforge.io/html/geodesic.html#geodseries

(defun horner (x &rest p)
  (loop with result = (first p)
        for a in (rest p)
        do (setf result (+ a (* result x)))
        finally (return result)))

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

;; equation (15) and (23)
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
  (+ tau
     (loop for c in c-prime-1
           for i from 1
           sum (* c (sin (* 2 i tau))))))

(defun direct (latitude azimuth distance)
  "LATITUDE and AZIMUTH in radians. DISTANCE in meters."
  (let* ((beta1 (reduce-latitude latitude))
         (alpha0 (alpha0 beta1 azimuth))
         (sigma1 (sigma beta1 azimuth))
         (omega1 (omega alpha0 sigma1))
         (square-k (let ((c (cos alpha0))) (* *square-e-prime* c c)))
         (epsilon (let ((c (sqrt (+ square-k 1)))) (/ (1- c) (1+ c))))
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
