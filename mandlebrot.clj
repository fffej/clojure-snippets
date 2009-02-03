;;; Draw a fractal
;;; jeff.foster@acm.org

(import '(javax.swing JFrame JPanel)
        '(java.awt Color)
	'(java.awt.image BufferedImage MemoryImageSource))

(def *max-iteration* 256)
(def *width* 128)
(def *height* 128)

(defn process-pixel [x y]
  ((fn [x y xc yc accum]
     (let [x1 (+ (- (* x x) (* y y)) xc)
	   y1 (+ (* 2 x y) yc)
	   sq (+ (* x1 x1) (* y1 y1))]
       (cond
	(> accum *max-iteration*) *max-iteration*
	(> sq 2.0) accum
	:else (recur x1 y1 xc yc (inc accum))))) x y x y 0))

(defn calculate-pixels ]
  (let [pixels (range 0 (* *width* *height*))]
    (pmap (fn [p] 
	    (let [row (rem p *width*) col (int (/ p *height*))]
	      (get-color (process-pixel (/ row (double *width*)) (/ col (double *height*))))))
	  pixels)))

(defn get-color [pixel]
  (Color/HSBtoRGB (/ (double pixel) *max-iteration*) 0.5 0.75))

(defn simple-mandlebrot [w h]
  (let [x (int-array (calculate-pixels))]
    (MemoryImageSource. w h x 0 w)))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)		  
    (doto g
      (.drawImage (.createImage this (simple-mandlebrot *width* *height*)) 0 0 nil)))))
      
(defn fractals []
  (let [frame (JFrame. "Fractals")]
    (doto frame
      (.add canvas)
      (.setSize 128 128)
      (.setVisible true))))