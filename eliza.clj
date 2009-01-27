;;; Eliza example from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org

;; Pattern matching bits and bobs
(defn variable? [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn segment-pattern? 
  "Is this a segment matching pattern: ((?* var) . pat)"
  [p]
  (and (seq? p)
       (.startsWith (first p) "?*")))

(defn atom? [x]
  (not (seq? x)))

(defn pat-match-v1
  "Does pattern match input?  Any variable can match anything."
  [pattern input]
  (if (variable? pattern)
    true
    (if (or (atom? pattern) (atom? input))
        (= pattern input)
	(and (pat-match-v1 (first pattern) (first input))
	     (pat-match-v1 (rest pattern) (rest input))))))

;; We differ from PAIP in just using a map which simplifies things
;; somewhat.  We use the empty map to signify a match that has no
;; bindings
(def no-bindings {})

(defn match-variable
  "Does VAR match input?  Uses (or updates) and returns bindings."
  [var input bindings]
  (let [binding (bindings var)]
    (cond
     (nil? binding) (assoc bindings var input)
     (= input binding) bindings
     :else nil)))

(defn pat-match-v2
  "Match pattern against input in the context of the bindings"
  ([pattern input] 
    (pat-match-v2 pattern input no-bindings))
  ([pattern input bindings]
    (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (and (seq? pattern) (seq? input))
       (pat-match-v2 
	(rest pattern) 
	(rest input)
	(pat-match-v2 (first pattern) (first input) bindings))
     :else nil)))

;; Segment pattern matching
(defn segment-pattern? 
  "Is this a segment matching pattern: ((?* var) . pattern)"
  [pattern]
  (and (seq? pattern) (seq? (first pattern)) (.startsWith (str (ffirst pattern)) "?*")))

(defn position [elt vec start]
  (if (>= start (count vec))
    nil
    (if (= (nth vec start) elt)
      start
      (recur elt vec (inc start)))))

(declare pat-match)

(defn sub-sequence 
  ([lst start] 
     (sub-sequence lst start (count lst)))
  ([lst start end]
  ((fn [lst x y accum]
     (if (>= x y)
       (reverse accum)
       (recur (rest lst) (inc x) y (conj accum (first lst))))) (nthrest lst start) 0 (- end start) '())))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern)) pat (rest pattern)]
       (if (nil? pat)
	   (match-variable var input bindings)
	   (let [pos (position (first pat) input start)]
	     (if pos
	       (let [b2 (pat-match pat (sub-sequence input pos) (match-variable var (sub-sequence input 0 pos) bindings))]
		 (if (nil? b2)
		   (segment-match pattern input bindings (inc pos))
		   b2))
	       nil))))))

(defn pat-match
  "Match pattern against input in the context of the bindings"
  ([pattern input]
     (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond
      (nil? bindings) nil
      (variable? pattern) (match-variable pattern input bindings)
      (= pattern input) bindings
      (segment-pattern? pattern) (segment-match pattern input bindings) ;;; To be determined
      (and (seq? pattern) (seq? input))
        (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings))
      :else nil)))
       
       

     
