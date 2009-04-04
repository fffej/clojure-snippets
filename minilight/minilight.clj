;;;; jeff.foster@acm.org
;;;; Still very early, so nothing exciting yet!
(ns fatvat.minilight
  (:import (java.io PushbackReader))
  (:use clojure.contrib.duck-streams))

;;;; http://www.hxa.name/minilight/ ray tracer in Clojure
;;; Description of algorithm
;;; 1. open model file
;;; 2. create Scene with model file
;;; 3. create Image with model file
;;; 4. create Camera with model file
;;; 5. do progressive render loop
;;;    1. get frame of render from Camera
;;;       1. do image sampling loop
;;;          1. make ray direction
;;;          2. get light value from RayTracer
;;;             1. get nearest intersection from Scene
;;;                1. get nearest intersection from SpatialIndex
;;;             2. get inward light from emitter sample
;;;             3. get inward light from next path step, by recursing
;;;             4. return total reflected light
;;;          3. set value to Image
;;;    2. get formatted from Image
;;;       1. apply simple tone-mapping
;;;    3. save formatted
;;; 6. exit


(defn dot 
  "Dot product of two vectors"
  [v1 v2] (reduce + (map * v1 v2)))

(defn cross
  "Cross product of two 3 vectors"
  [v1 v2]
  (vec
   (- (* (get v1 1) (get v2 2)) (get v1 2) (get v2 1))
   (- (* (get v1 2) (get v2 0)) (get v1 0) (get v2 2))
   (- (* (get v1 0) (get v2 1)) (get v1 1) (get v2 0))))

(defn unary-minus
  "Negation of a vector"
  [v] (vec (map - v)))

(defn unitize
  [v]
  (let [len (Math/sqrt (dot v v))
	overlen (if (not (zero? len)) (/ 1 len) 0)]
    (vec (map (fn [x] (* overlen x)) v))))

(defn between
  [mini maxi x]
  (min (max x mini) maxi))

(defn get-clamped
  "Clamp v within the bounds minimum and maximum"
  [v mini maxi]
  (vec (map between mini maxi v)))

(defn right-vector
  [model]
  (unitize (cross (:view-direction model) [0 1 0])))

(defn up-vector
  [model]
  (unitize (cross (:view-direction model) (right-vector model))))

(defstruct model
  :iterations
  :width
  :height
  :eye-position
  :look-direction
  :view-angle
  :sky-emission
  :ground-reflection
  :triangles)

(defstruct triangle
  :geometry
  :reflectivity
  :emitivity)

(defstruct image
  :height
  :width
  :pixels)

(defmacro safe-read [x]
  `(read ~x false nil))

(defn read-triangles
  [r]
  ((fn [triangles]
     (let [geom [(safe-read r) (safe-read r) (safe-read r)] refl (safe-read r) emit (safe-read r)]
       (if (first geom)
	 (recur (cons (struct triangle geom refl emit) triangles))
	 triangles))) nil))

(defn load-model
  [f]
  (with-open [r (PushbackReader. (reader f))]
    (.skip r (count "#MiniLight"))
    (struct model 
	    (read r) ;iterations
	    (read r) ;width
	    (read r) ;height
	    (read r) ;eye-position
	    (read r) ;look-direction
	    (read r) ;view-angle
	    (read r) ;sky-emission
	    (read r) ;ground-reflection
	    (read-triangles r)))) ;triangles

(defn create-image 
  [model]
  (let [w (:width model) h (:height model)]
    (struct image w h (vec (repeat (* w h) 0)))))

(defn add-pixel
  [image x y radiance]
  (let [w (:width image) h (:height image) p (:pixels image)]
  (struct image 
	  w
	  h
	  3)))

(defn get-sample-ray-direction
  "Make sample ray direction, stratified by pixels"
  [model image px py]
  (let [w (:width model)
	h (:height model)
	xf (- (* (/ 2.0 w) (+ px (rand))) 1)
	yf (- (* (/ 2.0 h) (+ py (rand))) 1)]
    1))
    


(defn render-point
  "Render the point in the model at (x,y) and return the radiance"
  [model px py]
  (let [xf (- (* (/ 2.0 (:width  model)) (+ px (rand))) 1)
	yf (- (* (/ 2.0 (:height model)) (+ py (rand))) 1)]
    4))
  

(defn render-iter
  [model image])
