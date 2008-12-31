(import '(javax.swing JFrame JButton JPanel Timer)
        '(java.awt.event ActionListener)
        '(java.awt Color BorderLayout))

(defn bubble [lst]
  (if (or (nil? lst) (nil? (second lst)))
    lst
    (if (> (first lst) (second lst))
      (lazy-cons (second lst) (lazy-cons (first lst) (bubble (nthrest lst 2))))
      (lazy-cons (first lst) (bubble (rest lst))))))

(defn bubble-sort [lst]
   (take (count lst) (iterate bubble lst)))

(def maxval 100)
(def initial-list (take 100 (repeatedly (fn [] (rand-int maxval)))))
(def position (atom 0))

(def canvas (proxy [JPanel ActionListener] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (.setColor g Color/RED)
    (let [width (.getWidth this) height (.getHeight this) bar-height (/ height (inc (count initial-list))) val-width (/ width maxval)
	  bubble-sorted (bubble-sort initial-list)]
      (doseq [val (into (sorted-map) (zipmap (range 0 (count initial-list)) (nth bubble-sorted @position)))] 
	(let [y (int (* (first val) bar-height)) barWidth (int (* (second val) val-width))]
	  (.fillRect g 0 y barWidth bar-height)))))
  (actionPerformed [e] (swap! position inc) (compare-and-set! position (count initial-list) 0) (.repaint this))))

(let [x (Timer. 50 canvas)]
  (defn stop-timer [] (.stop x))
  (defn start-timer [] (.start x))
  (defn is-running [] (.isRunning x)))

(defn sortapp []
  (let [frame (JFrame. "Sort Visualizer")run-button (JButton. "Go")]
    (.addActionListener run-button
      (proxy [ActionListener] []
	(actionPerformed [evt]
          (if (is-running) (stop-timer) (start-timer)))))
     (doto frame
      (.setLayout (BorderLayout. 3 3))
      (.add canvas (BorderLayout/CENTER))
      (.add run-button (BorderLayout/PAGE_END))
      (.setSize 300 300)
      (.setVisible true))))