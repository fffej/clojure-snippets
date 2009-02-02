;;; Draw a fractal
;;; jeff.foster@acm.org

(import '(javax.swing JFrame JPanel)
        '(java.awt Color)
	'(java.awt.image BufferedImage))

(def *max-iteration* 512)

(defn process-pixel [x y]
  ((fn [x y xc yc accum]
     (let [x1 (+ (- (* x x) (* y y)) xc)
	   y1 (+ (* 2 x y) yc)
	   sq (+ (* x1 x1) (* y1 y1))]
       (cond
	(> accum *max-iteration*) *max-iteration*
	(> sq 2.0) accum
	:else (recur x1 y1 xc yc (inc accum))))) x y x y 0))

(defn get-color [pixel]
  (* pixel 1024))

(defn simple-mandlebrot [w h]
  (let [img (BufferedImage. w h BufferedImage/TYPE_BYTE_INDEXED)]
    (doseq [x (range 0 w)]
      (doseq [y (range 0 h)]
	(let [pixel (process-pixel (double (/ x w)) (double (/ y h)))]
	  (.setRGB img x y (get-color pixel)))
	))
    img))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)		  
    (doto g
      (.setColor Color/RED)
      (.drawImage (simple-mandlebrot (.getWidth this) (.getHeight this)) 0 0 nil)))))
      
(defn fractals []
  (let [frame (JFrame. "Fractals")]
    (doto frame
      (.add canvas)
      (.setSize 256 256)
      (.setResizable false)
      (.setVisible true))))