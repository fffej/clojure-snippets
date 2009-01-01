;;; Implementation of ray tracing algorithm from ANSI Common Lisp
(import '(javax.swing JFrame JPanel)
        '(java.awt Color))


;; Math Utility functions
(defn square [x] (* x x))

(defn magnitude [x y z]
  (Math/sqrt (+ (square x) (square y) (square z))))

(defn unit-vector [x y z]
  (let [d (magnitude x y z)]
    [( / x d) (/ y d) (/ z d)]))

(defstruct point :x :y :z)

(defn distance [p1 p2]
  (magnitude
   (- (:x p1) (:x p2))
   (- (:y p1) (:y p2))
   (- (:z p1) (:z p2))))

(defn minroot [a b c]
  (if (zero? a)
    (/ (- c) b)
    (let [disc (- (square b) (* 4 a c))]
      (if (> 0 disc)
	(let [discroot (Math/sqrt disc)]
	  (min (/ (+ (- b) discroot) (* 2 a))
	       (/ (- (- b) discroot) (* 2 a))))))))


(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)		  
    (.setColor g Color/RED)
    (.fillRect g 0 0 300 300))))

(defn raytraceapp []
  (let [frame (JFrame. "Ray Tracing")]
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setVisible true))))
