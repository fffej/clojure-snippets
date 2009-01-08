;;; Regex-coach
;;; Inspired by http://www.weitz.de/regex-coach/
;;; jeff.foster@acm.org


(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel BoxLayout)
       '(java.awt.event ActionListener KeyAdapter)
       '(java.util.regex PatternSyntaxException)
       '(javax.swing.text DefaultHighlighter$DefaultHighlightPainter)
       '(java.awt GridLayout Color))

;; Should be a macro
(defn first-match [m]
  (if (coll? m) (first m) m))

(defn match [regex text]
  (let [m (first-match (re-find (re-pattern regex) text))]
    (if (nil? m)
      [0 0]
      (let [ind (.indexOf text m) len (.length m)]
	[ind (+ ind len)]))))

(defn regexcoach []
  (let [frame (JFrame. "Regular Expression Coach") pane (JPanel.) regexText (JTextField.) 
	targetText (JTextField. "")
	statusBar (JLabel. "Match from 0 to 0")
        keyHandler (proxy [KeyAdapter] [] 
		     (keyTyped [keyEvent] 
		       (try       
			(let [m (match (.getText regexText) (.getText targetText)) 
			      hl (.getHighlighter targetText)
			      pen (DefaultHighlighter$DefaultHighlightPainter. Color/RED)]
			  (.removeAllHighlights hl)
			  (.addHighlight hl (first m) (second m) pen)
			  (.setText statusBar (format "Match from %s to %s" (first m) (second m))))
			(catch PatternSyntaxException e (.setText statusBar (.getMessage e))))))]
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