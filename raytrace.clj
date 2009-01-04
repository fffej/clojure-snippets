;;; Implementation of ray tracing algorithm from ANSI Common Lisp
(import '(javax.swing JFrame JPanel)
        '(java.awt Color)
	'(java.awt.image BufferedImage))

;; Bits for the modelling


;; Math Utility functions
(defn square [x] (* x x))

(defstruct point :x :y :z)

(defn magnitude [p]
  (Math/sqrt (+ (square (:x p)) (square (:y p)) (square (:z p)))))

(defn unit-vector [p]
  (let [d (magnitude p)]
    (struct point (/ (:x p) d) (/ (:y p) d) (/ (:z p) d))))

(defn point-subtract [p1 p2]
  (struct point 
	  (- (:x p1) (:x p2))
	  (- (:y p1) (:y p2))
	  (- (:z p1) (:z p2))))

(defn distance [p1 p2]
  (magnitude (point-subtract p1 p2)))

(defn minroot [a b c]
  (if (zero? a)
    (/ (- c) b)
    (let [disc (- (square b) (* 4 a c))]
      (if (> disc 0)
	(let [discroot (Math/sqrt disc)]
	  (min (/ (+ (- b) discroot) (* 2 a))
	       (/ (- (- b) discroot) (* 2 a))))))))

;; Ray tracing bits
(def eye (struct point 150 150 200))

(defstruct surface :color)

(defstruct sphere :color :radius :centre) ;; Clojure doesn't appear to support include?

(defn defsphere [point r c]
  (struct sphere c r point))

(def world [(defsphere (struct point 150 150 -600) 250 0.32)
            (defsphere (struct point 175 175 -300) 100 0.64)])

(defn sphere-normal [s pt]
  (let [c (:centre s)]
    (unit-vector (point-subtract c pt))))

(defn sphere-intersect [s pt ray]
  (let [c (:centre s)
	n (minroot (+ (square (:x ray)) (square (:y ray)) (square (:z ray)))
		   (* 2 (+ (* (- (:x pt) (:x c)) (:x ray))
			   (* (- (:y pt) (:y c)) (:y ray))
			   (* (- (:z pt) (:z c)) (:z ray))))
		   (+ (square (- (:x pt) (:x c)))
		      (square (- (:y pt) (:y c)))
		      (square (- (:z pt) (:z c)))
		      (- (square (:radius s)))))]
	(if n
	  (struct point (+ (:x pt) (* n (:x ray)))
                        (+ (:y pt) (* n (:y ray)))
			(+ (:z pt) (* n (:z ray)))))))

(defn lambert [s intersection ray]
  (let [normal (sphere-normal s intersection)]
    (max 0 (+ (* (:x ray) (:x normal))
	      (* (:y ray) (:y normal))
	      (* (:z ray) (:z normal))))))

;; second item = what we hit
;; first item = where we hit
(defn first-hit [pt ray]
  (reduce 
   (fn [x y]
     (let [hx (first x) hy (first y)]
       (cond
	(nil? hx) y
	(nil? hy) x
	:else (let [d1 (distance hx pt) d2 (distance hy pt)]
		  (if (< d1 d2) x y)))))
   (map (fn [obj]
	  (let [h (sphere-intersect obj pt ray)]
	    (if (not (nil? h))
	      [h obj]))) world)))

(defn send-ray [src ray]
  (let [hit (first-hit src ray)]
    (if (not (nil? hit))
      (let [int (first hit) s (second hit)]
	(* (lambert s ray int) (:color s)))
      0)))

(defn color-at [x y]
  (let [ray (unit-vector (point-subtract (struct point x y 0) eye))]
    (* (send-ray eye ray) 255)))

(defn ray-trace [world res g w h ox oy]
  (let [buffered-image (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)]
    (doseq [x (range ox (+ ox w))]
      (doseq [y (range oy (+ oy h))]
	(.setRGB buffered-image x y (color-at x y))))
    buffered-image))

(defn create-work-list [width height unitX unitY]
  (let [xs (range 0 width unitX) ys (range 0 height unitY)]
    (mapcat (fn [x] (mapcat (fn [y] (list (list x y))) ys)) xs)))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)		  
    (.setColor g Color/RED)
    (let [width (.getWidth this) height (.getHeight this) buffered-image (ray-trace world 1 g width height 0 0) unitX (/ width 4) unitY (/ height 4)]
      

      (.drawImage g buffered-image 0 0 Color/RED nil)))))

(defn raytraceapp []
  (let [frame (JFrame. "Ray Tracing")]
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable false)
      (.setVisible true))))

;;[21:53] <Chousuke> fffej: you could add a higher-order function "point-op" that takes an [op & points] and returns (struct point (apply op (map :x points)) (apply op (map :y points)) (apply op (map :z points)))
;; [21:54] <Chousuke> fffej: dunno what that does to speed, but it might make it clearer :)