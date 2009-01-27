;;; jeff.foster@acm.org

(import '(javax.swing JFrame JPanel)
       '(java.awt.image BufferedImage)
       '(java.awt Color))

(def world-size 16)

(defstruct snake :position :direction)

(defstruct cell :color)

(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn world []
     (apply vector 
            (map (fn [_] 
                   (apply vector (map (fn [_] (ref (struct cell Color/BLUE))) 
                                      (range world-size)))) 
                 (range world-size))))

(defn place [w [x y]]
  (-> w (nth x) (nth y)))

(defn render [w width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
	g (.getGraphics img)
	sq-size (/ (min width height) world-size)]
    (doseq [x (range 0 world-size)]
      (doseq [y (range 0 world-size)]
	(let [sq (place w [x y])]
	  (.setColor g (sq :color))
	  (.fillOval g (* x sq-size) (* y sq-size) sq-size sq-size))))
    img))

(defn bound 
  "returns n wrapped into range 0-b"
  [b n]
    (let [n (rem n b)]
      (if (neg? n) 
        (+ n b) 
        n)))

(defn create-snake [x y world color]
  (sync
      (let [p (place world [x y])]
	(alter p assoc :color Color/RED))))

(defn delta-loc 
  "returns the location one step in the given dir. Note the world is a torus"
  [[x y] dir]
    (let [[dx dy] (dir-delta (bound 8 dir))]
      [(bound world-size (+ x dx)) (bound world-size (+ y dy))]))

(defn snake-behaviour [[state world]]
  (let [new-pos (delta-loc (state :position) (state :direction))]
    (prn new-pos)))


;;; GUI gibberish
(defn amazing []
  (let [w (world)
	frame (JFrame. "A-maze-ing")
	canvas (proxy [JPanel] []
	  (paintComponent [g]		  
	    (proxy-super paintComponent g)
	    (let [square (min (.getWidth this) (.getHeight this))]
	      (.drawImage g 
			  (render w square square)
			  0 0 Color/WHITE nil))))]
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable true)
      (.setVisible true))))

