;;; jeff.foster@acm.org
;;; Adaption of maze algorithm from
;;; http://en.wikipedia.org/wiki/Maze_generation_algorithm
;;; http://en.wikipedia.org/wiki/Prim%27s_algorithm

(defstruct maze-cell :wall-left :wall-right :wall-top :wall-bottom :in-maze)

(def other
     {:wall-left :wall-right, :wall-right :wall-left, :wall-top :wall-bottom, :wall-bottom :wall-top})

(defn full-cell []
  (struct maze-cell true true true true false))

(def maze-size 40)

(defn world [x y]
  (apply vector 
	 (map (fn [_] 
		(apply vector (map (fn [_] (ref (full-cell)))
				   (range x)))) 
	      (range y))))

(defn maze-at [maze pos]
  (-> maze (nth (first pos)) (nth (second pos))))

(defn walls [grid pos]
  (map (fn [x] [(first x) (maze-at grid pos)]) 
       (filter (fn [x] (and (not (= (first x) :in-maze)) (second x))) 
	       @(maze-at grid pos))))

(defn opposite-cell 
  "This is a scarily bad function"
  [grid pos wall]
  (let [p (get {:wall-top [0 -1] :wall-left [-1 0] :wall-right [1 0] :wall-bottom [0 1]} wall)
	new-pos [(+ (first pos) (first p)) (+ (second pos) (second p))]
	new-x (first new-pos)
	new-y (second new-pos)]
    (when (and (>= new-x 0) (< new-x maze-size) (>= new-y 0) (< new-y maze-size))
      new-pos)))

(defn random-wall [w]
  (nth w (rand-int (count w))))
	    
(defn prims-algorithm [grid pos wall-list]
  (if (empty? wall-list) ;; while there are walls in the cell
    grid
    (let [cell (maze-at grid pos)
	  random-wall-cell (random-wall wall-list)
	  w (first random-wall-cell)
	  opposite (opposite-cell grid pos w)
	  opposite-valid (and opposite (= false (get @(maze-at grid opposite) :in-maze)))] ;; pick random wall from list
      (when opposite-valid
	(prn "Opposite is valid" w)
	(dosync 
	 (alter cell assoc w false) ; make the wall a passage
	 (alter (maze-at grid opposite) assoc (other w) false) ; mark the opposite wal
	 (alter (maze-at grid opposite) assoc :in-maze true) ; mark the cell on the opposite part of the maze
	 (alter cell assoc :in-maze true))) ; mark the current cell
      (prn @cell)
      (let [removed-current-wall (remove (partial = random-wall-cell) wall-list)]
	(assert (= -1 (- (count removed-current-wall) (count wall-list))))
	(if opposite-valid
	  (recur grid opposite (concat removed-current-wall (walls grid opposite)))
	  (recur grid pos removed-current-wall))))))

(defn generate-maze [x]
  (let [grid (world x x)]
    (dosync
     (alter (maze-at grid [0 0]) assoc :in-maze true))
    ;; pick a cell, mark it as part of the maze and add the walls of the cell to the wall list
    (prims-algorithm grid [0 0] (walls grid [0 0]))
    grid))

;;; GUI rubbish
(import '(javax.swing JFrame JPanel)
	'(java.awt.image BufferedImage)
	'(java.awt Color))

(defn draw-maze [maze sq]
  (let [img (BufferedImage. sq sq BufferedImage/TYPE_INT_ARGB)
	sq-size ( / sq maze-size)
	g (.getGraphics img)]
    (.setColor g Color/BLUE)
    (doseq [x (range 0 maze-size)]
      (doseq [y (range 0 maze-size)]
	(let [cell (maze-at maze [x y]) left (* x sq-size) top (* y sq-size) right (+ left sq-size) bottom (+ top sq-size)]
	  (when (cell :wall-top)    (.drawLine g left top right top))
	  (when (cell :wall-bottom) (.drawLine g left bottom right bottom))
	  (when (cell :wall-left)   (.drawLine g left top left bottom))
	  (when (cell :wall-right)  (.drawLine g right top right bottom)))))
	   
    img))

(defn amazing []
  (let [maze (generate-maze maze-size)
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