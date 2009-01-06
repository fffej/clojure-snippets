;;; Regex-coach
;;; Inspired by http://www.weitz.de/regex-coach/
;;; jeff.foster@acm.org


(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel BoxLayout)
       '(java.awt.event ActionListener KeyAdapter)
       '(java.awt GridLayout Color))


(defn regexcoach []
  (let [frame (JFrame. "Regular Expression Coach") pane (JPanel.) regexText (JTextField.) 
	targetText (JTextField.)
	statusBar (JLabel. "Match from 0 to 0")
        keyHandler (proxy [KeyAdapter] [] (keyTyped [keyEvent] (prn keyEvent))) ]
    (doto regexText
      (.addKeyListener keyHandler))
    (doto targetText
      (.addKeyListener keyHandler))
    (doto pane
      (.setLayout (BoxLayout. pane BoxLayout/Y_AXIS))
      (.add (JLabel. "Regular Expression"))
      (.add regexText)
      (.add (JLabel. "Target String"))
      (.add targetText)
      (.add statusBar))
    (doto frame
      (.add pane)
      (.setSize 300 300)
      (.setVisible true))))