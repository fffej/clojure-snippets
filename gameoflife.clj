;;; Implementation of the Game of Life
;;; jeff.foster@acm.org

(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
       '(java.awt.event ActionListener MouseListener MouseAdapter)
       '(java.awt GridLayout Color))


;; Rules from Wikipedia
;; 1. Any live cell with fewer than two live neighbours dies, as if by needs caused by underpopulation.
;; 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
;; 4. Any tile with exactly three live neighbours cells will be populated with a living cell.

(defn world-at [world x y]
  (if (and (>= x 0) (>= y 0) (< x (count world)) (< y (count (first world))))
    (nth (nth world x) y)
    0))

(defn neighbour-count [world x y]
  (+ (world-at world (dec x) (dec y)) (world-at world x (dec y)) (world-at world (inc x) (dec y))
     (world-at world (dec x) y) (world-at world (inc x) y)
     (world-at world (dec x) (inc y)) (world-at world x (inc y)) (world-at world (inc x) (inc y))))

(defn new-state [world x y]
  (let [neighbours (neighbour-count world x y) alive (world-at world x y)]
    (cond 
     (and alive (< 2 neighbours)) 0 ;; under population
     (and alive (> 3 neighbours)) 0 ;; over-crowding
     (and alive (or (= 2 neighbours) (= 3 neighbours))) 1 ;; unchanged to the next generation
     (and (not alive) (= 3 neighbours)) 1)))
      

(defn create-world [w h]
  (let [row (into (vector) (replicate w 0))]
    (into (vector) (replicate h row))))

(defn life-step [world]
  (let [width (count world) height (count (first world))]
    (map 
     (fn [row] (map (fn [col] 
		      (let [x (first row) y (first col)]
			(new-state world x y)))
		    (zipmap (range 0 height) (second row))))
     (zipmap (range 0 width) world))))

;; UI elements and mutable ness

(def grid-size 16)

(def world (atom (create-world grid-size grid-size)))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (doseq [x (range 0 grid-size)]
      (doseq [y (range 0 grid-size)]
	(let [alive (world-at @world x y)]
	  (cond
	   (zero? alive) (.setColor g Color/BLUE)
	   (:else) (.setColor g Color/RED))
	  (.fillRect g (* x grid-size) (* y grid-size) (dec grid-size) (dec grid-size))))))))

(defn lifeapp []
  (let [frame (JFrame. "Game of Life")]
    (doto canvas
      (.addMouseListener (proxy [MouseAdapter] []
        (mouseClicked [e] 
	  (let [x (int (/ (.getX e) grid-size)) y (int (/ (.getY e) grid-size))]
	    (prn x y))))))
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable false)
      (.setVisible true))))