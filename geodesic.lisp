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

;; equation (17)
(defun a-1 (epsilon)
  (/ (1+ (loop for i in '(2 6 8 14 16)
               for j from 2 by 2
               sum (/ (expt epsilon j) (expt 2 i))))
     (- 1 epsilon)))

;; equation (18)
(defun c-1 (epsilon)
  (let ((e (make-array 11 :initial-contents (loop for i below 11 collect (expt epsilon i)))))
    (list (+ (* -1/2 epsilon)
             (* 3/16 (aref e 3))
             (* -1/32 (aref e 5))
             (* 19/2048 (aref e 7))
             (* -3/4096 (aref e 9)))
          (+ (* -1/16 epsilon epsilon)
             (* 1/32 (aref e 4))
             (* -9/2048 (aref e 6))
             (* 7/4096 (aref e 8))
             (* 1/65536 (aref e 10)))
          (+ (* -1/48 (aref e 3))
             (* 3/256 (aref e 5))
             (* -3/2048 (aref e 7))
             (* 17/24576 (aref e 9)))
          (+ (* -5/512 (aref e 4))
             (* 3/512 (aref e 6))
             (* -11/16384 (aref e 8))
             (* 3/8192 (aref e 10)))
          (+ (* -7/1280 (aref e 5))
             (* 7/2048 (aref e 7))
             (* -3/8192 (aref e 9)))
          (+ (* -7/2048 (aref e 6))
             (* 9/4096 (aref e 8))
             (* -117/524288 (aref e 10)))
          (+ (* -33/14336 (aref e 7))
             (* 99/65536 (aref e 9)))
          (+ (* -429/262144 (aref e 8))
             (* 143/131072 (aref e 10)))
          (+ (* -715/589824 (aref e 9)))
          (+ (* -2431/2621440 (aref e 10))))))

;; equation (15) and (23)
(defun i (sigma an cn)
  (* an (+ sigma (loop for c in cn
                       for i from 1
                       sum (* c (sin (* 2 i sigma)))))))

;; equation (21)
(defun c-prime-1 (epsilon)
  (let ((e (make-array 11 :initial-contents (loop for i below 11 collect (expt epsilon i)))))
    (list (+ (* 1/2 epsilon)
             (* -9/32 (aref e 3))
             (* 205/1536 (aref e 5))
             (* -4879/73728 (aref e 7))
             (* 9039/327680 (aref e 9)))
          (+ (* 5/16 (aref e 2))
             (* -37/96 (aref e 4))
             (* 1335/4096 (aref e 6))
             (* -86171/368640 (aref e 8))
             (* 4119073/28311552 (aref e 10)))
          (+ (* 29/96 (aref e 3))
             (* -75/128 (aref e 5))
             (* 2901/4096 (aref e 7))
             (* -443327/655360 (aref e 9)))
          (+ (* 539/1536 (aref e 4))
             (* -2391/2560 (aref e 6))
             (* 1082857/737280 (aref e 8))
             (* -2722891/1548288 (aref e 10)))
          (+ (* 3467/7680 (aref e 5))
             (* -28223/18432 (aref e 7))
             (* 1361343/458752 (aref e 9)))
          (+ (* 38081/61440 (aref e 6))
             (* -733437/286720 (aref e 8))
             (* 10820079/1835008 (aref e 10)))
          (+ (* 459485/516096 (aref e 7))
             (* -709743/163840 (aref e 9)))
          (+ (* 109167851/82575360 (aref e 8))
             (* -550835669/74317824 (aref e 10)))
          (+ (* 83141299/41287680 (aref e 9)))
          (+ (* 9303339907/2972712960 (aref e 10))))))

;; equation (24) and (25)
(let* ((n *n*)
       (n2 (* n n))
       (n3 (expt n 3))
       (n4 (expt n 4)))
  (defparameter *F-a-3*
    (make-array 10 :initial-contents
                (list 0
                      (- 1/2 (/ n 2))
                      (+ 1/4 (/ n 8) (* -3/8 n2))
                      (+ 1/16 (* 3/16 n) (/ n2 16) (* -5/16 n3))
                      (+ 3/64 (/ n 32) (* 5/32 n2) (* 5/128 n3) (* -35/128 n4))
                      (+ 3/128 (* 5/128 n) (* 5/256 n2) (* 35/256 n3) (* 7/256 n4))
                      (+ 5/256 (* 15/1024 n) (* 35/1024 n2) (* 7/512 n3))
                      (+ 25/2048 (* 35/2048 n) (* 21/2048 n2))
                      (+ 175/16384 (* 35/4096 n))
                      245/32768))
  "Factors for A-3.")
  (defparameter *P-c-3*
    (make-array '(9 10) :initial-contents
                (list (list 0
                            (- 1/4 (/ n 4))
                            (- 1/8 (/ n2 8))
                            (+ 3/64 (* 3/64 n) (* -1/64 n2) (* -5/64 n3))
                            (+ 5/128 (/ n 64) (/ n2 64) (* -1/64 n3) (* -7/128 n4))
                            (+ 3/128 (* 11/512 n) (* 3/512 n2) (/ n3 256) (* -7/512 n4))
                            (+ 21/1024 (* 5/512 n) (* 13/1024 n2) (/ n3 512))
                            (+ 243/16384 (* 189/16384 n) (* 83/16384 n2))
                            (+ 435/32768 (* 109/16384 n))
                            345/32768)
                      (list 0 0
                            (+ 1/16 (* -3/32 n) (/ n2 32))
                            (+ 3/64 (* -1/32 n) (* -3/64 n2) (/ n3 32))
                            (+ 3/128 (/ n 128) (* -9/256 n2) (* -3/128 n3) (* 7/256 n4))
                            (+ 5/256 (/ n 256) (* -1/128 n2) (* -7/256 n3) (* -3/256 n4))
                            (+ 27/2048 (* 69/8192 n) (* -39/8192 n2) (* -47/4096 n3))
                            (+ 187/16384 (* 39/8192 n) (* 31/16384 n2))
                            (+ 287/32768 (* 47/8192 n))
                            255/32768)
                      (list 0 0 0
                            (+ 5/192 (* -3/64 n) (* 5/192 n2) (* -1/192 n3))
                            (+ 3/128 (* -5/192 n) (* -1/64 n2) (* 5/192 n3) (* -1/128 n4))
                            (+ 7/512 (* -1/384 n) (* -77/3072 n2) (* 5/3072 n3) (* 65/3072 n4))
                            (+ 3/256 (* -1/1024 n) (* -71/6144 n2) (* -47/3072 n3))
                            (+ 139/16384 (* 143/49152 n) (* -383/49152 n2))
                            (+ 243/32768 (* 95/49152 n))
                            581/98304)
                      (list 0 0 0 0
                            (+ 7/512 (* -7/256 n) (* 5/256 n2) (* -7/1024 n3) (/ n4 1024))
                            (+ 7/512 (* -5/256 n) (* -7/2048 n2) (* 9/512 n3) (* -21/2048 n4))
                            (+ 9/1024 (* -43/8192 n) (* -129/8192 n2) (* 39/4096 n3))
                            (+ 127/16384  (* -23/8192 n) (* -165/16384 n2))
                            (+ 193/32768 (* 3/8192 n))
                            171/32768)
                      (list 0 0 0 0 0
                            (+ 21/2560 (* -9/512 n) (* 15/1024 n2) (* -7/1024 n3) (* 9/5120 n4))
                            (+ 9/1024 (* -15/1024 n) (* 3/2048 n2) (* 57/5120 n3))
                            (+ 99/16384 (* -91/16384 n) (* -781/81920 n2))
                            (+ 179/32768 (* -55/16384 n))
                            141/32768)
                      (list 0 0 0 0 0 0
                            (+ 11/2048 (* -99/8192 n) (* 275/24576 n2) (* -77/12288 n3))
                            (+ 99/16384 (* -275/24576 n) (* 55/16384 n2))
                            (+ 143/32768 (* -253/49152 n))
                            33/8192)
                      (list 0 0 0 0 0 0 0
                            (+ 429/114688 (* -143/16384 n) (* 143/16384 n2))
                            (+ 143/32768 (* -143/16384 n))
                            429/131072)
                      (list 0 0 0 0 0 0 0 0
                            (+ 715/262144 (* -429/65536 n))
                            429/131072)
                      (list 0 0 0 0 0 0 0 0 0 2431/1179648)))
    "Polynomial factors for C-3."))

(defun ac-3 (epsilon)
  (let* ((k (array-dimension *P-c-3* 1))
         (e (make-array k :initial-contents (loop for i below k
                                                  collect (expt epsilon i)))))
    (values
     (- 1
        (loop for i below (array-dimension *F-a-3* 0)
              sum (* (aref *F-a-3* i) (aref e i)))
     (* (aref *F-a-3* 0) epsilon)
     (* (aref *F-a-3* 1) epsilon epsilon)
     (* (aref *F-a-3* 2) (expt epsilon 3))
     (* (aref *F-a-3* 3) (expt epsilon 4))
     (* 3/128 (expt epsilon 5)))
     (loop for i below (array-dimension *P-c-3* 0)
           collect (loop for j below k
                         sum (* (aref *P-c-3* i j) (aref e j)))))))

;; equation (20)
(defun sigma-tau (tau c-prime-1)
  (+ tau
     (loop for c in c-prime-1
           for i from 1
           sum (* c (sin (* 2 i tau))))))

(defun direct (latitude azimuth distance)
  "LATITUDE and AZIMUTH in radians. DISTANCE in meters."
  (let* ((beta (reduce-latitude latitude))
         (alpha0 (alpha0 beta azimuth))
         (sigma1 (sigma beta azimuth))
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
