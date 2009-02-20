(ns jmusic
  (:use [clojure.contrib.import-static :only (import-static)])
  (:use [game-of-life])
  (:import jm.JMC)
  (:import (jm.util Write))
  (:import (jm.music.data Note Score Part Phrase)))

(import-static jm.JMC 
	       CROTCHET
	       C4
	       FLUTE)

(defmacro jm-add-children
  [m obj parts]
  `(let [obj# ~obj]
      (doseq [p# ~parts]
	(doto obj#
	  (~m p#)))
      obj#))

(defn make-score 
  [name parts]
  (let [sc (Score. name)]
    (jm-add-children .addPart sc parts)))

(defn make-phrase
  [name notes]
  (let [p (Phrase. name)]
    (jm-add-children .addNote p notes)))

(defn make-part 
  [name instrument phrases]
  (let [part (Part. name instrument)]
    (jm-add-children .addPhrase part phrases)))

(defn make-note
  [freq rhythm]
  (Note. freq rhythm))

(defn save [score output]
  (Write/midi score output))

(defn make-noise []
  (let [notes (map (fn [y] (make-note (+ C4 y) CROTCHET)) (range 0 12))
	phrase (make-phrase "Phrase1" notes)
	part   (make-part "Part" FLUTE (list phrase))
	score (make-score "Score" (list part))]
    (save score "chromatic-scale.mid")))
