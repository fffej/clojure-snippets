;;; Implementation of the Game of Life
;;; jeff.foster@acm.org

(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
       '(java.awt.event ActionListener MouseListener MouseAdapter MouseEvent)
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

(defn toggle [x]
  (if (= x 0) 1 0))

(defn toggle-row-at [row pos]
  (map (fn [x] (if (= pos (first x)) (toggle (second x)) (second x))) (zipmap (range 0 (count row)) row)))

(defn toggle-pos [world x y]
  (map (fn [v] (if (= (first v) x) 
		   (toggle-row-at (second v) y)
	           (second v))) 
       (zipmap (range 0 (count world)) world)))


(defn neighbour-count [world x y]
  (+ (world-at world (dec x) (dec y)) (world-at world x (dec y)) (world-at world (inc x) (dec y))
     (world-at world (dec x) y) (world-at world (inc x) y)
     (world-at world (dec x) (inc y)) (world-at world x (inc y)) (world-at world (inc x) (inc y))))

(defn new-state [world x y]
  (let [neighbours (neighbour-count world x y) alive (world-at world x y)]
    (cond 
     (and (= alive 1) (< neighbours 2)) 0 ;; under population
     (and (= alive 1) (> neighbours 3)) 0 ;; over-crowding
     (and (= alive 1) (or (= 2 neighbours) (= 3 neighbours))) 1 ;; unchanged to the next generation
     (and (= 3 neighbours)) 1 ;; any tile with exactly 3 live neighbour cells becomes alive
     :else 0)))

(defn create-world [w h]
  (replicate h (replicate w 0)))

(defn life-step [w]
  (let [width (count w) height (count (first w))]
    (map 
     (fn [row] (map (fn [col] 
		      (let [x (first row) y (first col)]
			(new-state w x y)))
		    (zipmap (range 0 height) (second row))))
     (zipmap (range 0 width) w))))

;; UI elements and mutable ness

(def grid-size 15)

(def *world* (atom (create-world grid-size grid-size)))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (doseq [x (range 0 grid-size)]
      (doseq [y (range 0 grid-size)]
	(let [alive (world-at @*world* x y) sq-size (/ (min (.getHeight this) (.getWidth this)) grid-size)]
	  (cond
	   (zero? alive) (.setColor g Color/BLUE)
	   :else (.setColor g Color/RED))
	  (.fillRect g (* x sq-size) (* y sq-size) (dec sq-size) (dec sq-size))))))))

(defn lifeapp []
  (swap! *world* (fn [w] (create-world grid-size grid-size))) 
  (let [frame (JFrame. "Game of Life")]
    (doto canvas
      (.addMouseListener (proxy [MouseAdapter] []
        (mouseClicked [e] 
          (if (= (MouseEvent/BUTTON1) (.getButton e))
	    (let [sq-size (/ (min (.getHeight canvas) (.getWidth canvas)) grid-size) x (int (/ (.getX e) sq-size)) y (int (/ (.getY e) sq-size))]
	      (swap! *world* (fn [w] (toggle-pos w x y))))
	    (swap! *world* (fn [w] (life-step w))))
	  (.repaint canvas)))))
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable true)
      (.setVisible true))))