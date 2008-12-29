(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
        '(java.awt.event ActionListener)
        '(java.awt Color GridLayout))

(defn draw-sort [canvas alg]
  (prn alg))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (.setColor g Color/RED)
    (let [x (.getWidth this) y (.getHeight this)]
      (prn x y)))))

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
	
