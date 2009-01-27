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

(defn pat-match
  "Match pattern against input in the context of the bindings"
  ([pattern input] 
    (pat-match pattern input no-bindings))
  ([pattern input bindings]
    (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (and (seq? pattern) (seq? input))
       (pat-match 
	(rest pattern) 
	(rest input)
	(pat-match (first pattern) (first input) bindings))
     :else nil)))

       

     
