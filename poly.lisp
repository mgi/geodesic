(in-package :geodesic)

(defun horner (x &rest p)
  "Horner's method of P at X."
  (loop with result = (first p)
        for a in (rest p)
        do (setf result (+ a (* result x)))
        finally (return result)))

;; Polynomials of degrees 3 and 4
;;
;; (see: https://www.1728.org/quartic2.htm or
;; https://dlmf.nist.gov/1.11)
(defun 3rt (number)
  "Cubic root that handles negative numbers."
  (if (minusp number)
      (- (expt (abs number) (/ 3)))
      (expt number (/ 3))))

(defun cubic-roots (a b c d)
  "Roots of a·z³ + b·z² + c·z + d."
  (let* ((f (/ (- (/ (* 3 c) a) (/ (* b b) (* a a))) 3))
         (g (/ (+ (/ (* 2 b b b) (* a a a)) (- (/ (* 9 b c) (* a a))) (/ (* 27 d) a)) 27))
         (h (+ (/ (* g g) 4) (/ (* f f f) 27))))
    (cond ((minusp h)
           ;; 3 distinct real roots
           (let* ((i (sqrt (- (/ (* g g) 4) h)))
                  (j (3rt i))
                  (k (acos (- (/ g (* 2 i)))))
                  (l (- j))
                  (k/3 (/ k 3))
                  (m (cos k/3))
                  (n (* (sqrt 3) (sin k/3)))
                  (p (- (/ b (* 3 a)))))
             (list (+ (* 2 j m) p)
                   (+ (* l (+ m n)) p)
                   (+ (* l (- m n)) p))))
          ((plusp h)
           ;; only one real root
           (let* ((r (- (sqrt h) (/ g 2)))
                  (s (3rt r))
                  (u (- (3rt (+ (/ g 2) (sqrt h)))))
                  (real (- (+ (/ (+ s u) 2) (/ b (* 3 a)))))
                  (imag (* (- s u) (/ (sqrt 3) 2))))
             (list (- (+ s u) (/ b (* 3 a)))
                   (complex real imag)
                   (complex real (- imag)))))
          ((zerop h)
           ;; 3 equal real roots
           (let ((x (- (3rt (/ d a)))))
             (list x x x))))))

(defun best-2-roots (roots)
  (let ((nonzero (remove-if #'zerop roots)))
    (if (= 2 (count-if #'complexp nonzero))
        (remove-if-not #'complexp nonzero)
        (subseq nonzero 0 2))))

(defun quartic-roots (a b c d e)
  (let* ((b-init b)
         (b (/ b a))
         (c (/ c a))
         (d (/ d a))
         (e (/ e a))
         (f (- c (/ (* 3 b b) 8)))
         (g (+ d (/ (* b b b) 8) (- (/ (* b c) 2))))
         (h (+ e (* b b (/ c 16)) (- (/ (* 3 b b b b) 256)) (- (/ (* b d) 4)))))
    (let* ((roots (cubic-roots 1 (/ f 2) (/ (- (* f f) (* 4 h)) 16) (- (/ (* g g) 64))))
           (best (best-2-roots roots))
           (p (sqrt (first best)))
           (q (sqrt (second best)))
           (r (- (/ g (* 8 p q))))
           (s (/ b-init (* 4 a))))
      (list (+ p q r (- s))
            (- p q r s)
            (- q p r s)
            (- r p q s)))))
