;;; jeff.foster@acm.org
;;; Adaption of maze algorithm from
;;; http://en.wikipedia.org/wiki/Maze_generation_algorithm
;;; http://en.wikipedia.org/wiki/Prim%27s_algorithm

(defstruct maze-cell :wall-left :wall-right :wall-top :wall-bottom :in-maze)

(def maze-size 20)

(defn world [x y]
  (apply vector 
	 (map (fn [_] 
		(apply vector (map (fn [_] (ref (full-cell)))
				   (range x)))) 
	      (range y))))

(defn maze-at [maze x y]
  (-> maze (nth x) (nth y)))

(defn full-cell []
  (struct maze-cell true true true true false))

(defn random-wall [cell]
  (let [walls (map first (filter (fn [x] (second x)) cell))]
    (prn walls)
    (nth walls (rand-int (count walls)))))

(defn generate-maze [x y]
  (let [grid (world x y)]
    grid))

;;; GUI rubbish
(import '(javax.swing JFrame JPanel)
	'(java.awt.image BufferedImage)
	'(java.awt Color))

(defn draw-maze [maze sq]
  (let [img (BufferedImage. sq sq BufferedImage/TYPE_INT_ARGB)
	sq-size ( / sq maze-size)
	g (.getGraphics img)]
    (doseq [x (range 0 maze-size)]
      (doseq [y (range 0 maze-size)]
	(let [cell (maze-at maze x y) left (* x sq-size) top (* y sq-size) right (+ left sq-size) bottom (+ top sq-size)]
	  (when (cell :wall-top)    (.drawLine g left top right top))
	  (when (cell :wall-bottom) (.drawLine g left bottom right bottom))
	  (when (cell :wall-left)   (.drawLine g left top left bottom))
	  (when (cell :wall-right)  (.drawLine g right top right bottom)))))
	   
    img))

(defn amazing []
  (let [maze (generate-maze maze-size maze-size)
	frame (JFrame. "A-maze-ing")
	canvas (proxy [JPanel] []
		 (paintComponent [g]
                   (proxy-super paintComponent g)
		   (.drawImage g 
			       (draw-maze maze (min (.getWidth this) (.getHeight this)))
			       0 0 Color/RED nil)))]
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable true)
      (.setVisible true))))