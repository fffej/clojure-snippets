(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
        '(java.awt.event ActionListener)
        '(java.awt Color GridLayout))

;; Not so good quick sort
(defn qsort [lst]
  (if (nil? lst)
    lst
    (concat
     (qsort (filter (partial > (first lst)) (rest lst)))
     (list (first lst))
     (qsort (filter (partial <= (first lst)) (rest lst))))))


(defn bubble [lst]
  (if (or (nil? lst) (nil? (second lst)))
    lst
    (if (> (first lst) (second lst))
      (lazy-cons (second lst) (lazy-cons (first lst) (bubble (nthrest lst 2))))
      (lazy-cons (first lst) (bubble (rest lst))))))

;; Bubble sort
(defn bubble-sort [lst]
   (take (count lst) (iterate bubble lst)))


(def maxval 100)

(def model (take 100 (repeatedly (fn [] (rand-int maxval)))))

(def bubble-sorted (bubble-sort model))

(def position (atom 0))

(def canvas (proxy [JPanel ActionListener] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (.setColor g Color/RED)
    (let [width (.getWidth this) height (.getHeight this) barHeight (/ height (inc (count model))) barWidthPerVal (/ width maxval)]
      (if (< @position (count model))
      (doseq [val (into (sorted-map) (zipmap (range 0 (count model)) (nth bubble-sorted @position)))] 
	(let [y (int (* (first val) barHeight)) barWidth (int (* (second val) barWidthPerVal))]
	  (.fillRect g 0 y barWidth barHeight))))))
  (actionPerformed [e] (swap! position inc) (.repaint this))))

(let [x (Timer. 250 canvas)]
  (defn stop-timer [] (.stop x))
  (defn start-timer [] (.start x))
  (defn is-running [] (.isRunning x)))

(defn draw-sort [canvas alg]
  (if (is-running)
    (stop-timer)
    (start-timer)))

(defn sortapp []
  (let [frame (JFrame. "Sort Visualizer")
	algorithm-chooser (JComboBox.)
	run-button (JButton. "Run Algorithm")]
    (.addActionListener run-button
      (proxy [ActionListener] []
	(actionPerformed [evt] 
	 (draw-sort canvas (.getSelectedItem algorithm-chooser)))))
    (doto algorithm-chooser
      (.addItem "Quick sort")
      (.addItem "Bubble sort"))
    (doto frame
      (.setLayout (GridLayout. 2 2 3 3))
      (.add algorithm-chooser)
      (.add canvas)
      (.add run-button)
      (.setSize 300 300)
      (.setVisible true))))
	
