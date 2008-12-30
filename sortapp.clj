(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
        '(java.awt.event ActionListener)
        '(java.awt Color GridLayout))

(def maxval 100)

(def model (take 100 (repeatedly (fn [] (rand-int maxval)))))

(def canvas (proxy [JPanel ActionListener] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (.setColor g Color/RED)
    (let [width (.getWidth this) height (.getHeight this) barHeight (/ height (inc (count model))) barWidthPerVal (/ width maxval)]
      (prn width height)
      (doseq [val (into (sorted-map) (zipmap (range 0 (count model)) model))] 
	(let [y (int (* (first val) barHeight)) barWidth (int (* (second val) barWidthPerVal))]
	  (.fillRect g 0 y barWidth barHeight)))))
  (actionPerformed [e] (prn "Doing something"))))

(let [x (Timer. 1000 canvas)]
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
	
