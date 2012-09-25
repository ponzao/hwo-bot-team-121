(ns pingpong.analyze
  (:use clojure.pprint)
  (:import java.io.File)
  (:require [pingpong.calc :as calc]))

(defn load-game 
  [file] 
  (drop 2 (read-string (slurp (.. file getAbsolutePath)))))

(def paddle-height 50)
(def paddle-width  10)
(def ball-radius   5)

(defn x-y-paddle 
  [{:keys [ball left conf]}]
  [(-> ball :pos :x)
   (-> ball :pos :y)
   (-> left :y)]) 

(defn split-chunks 
  [n s]
  (when 
    (and (seq s) (> (count s) (dec n)))
    (cons (take n s) (split-chunks n (rest s)))))

(defn turnpoint? 
  [[x1 x2 x3 x4]]
  (and x1 x2 x3 x4 (> x1 x2) (> x4 x3)))

(defn turnpoints 
  [game]
  "Returns four [x y paddlehit] positions for each left player turnpoints"
  (filter 
    #(turnpoint? (map first %)) 
    (split-chunks 4 (map x-y-paddle game))))

(defn turnangle
  [[p1 p2 p3 p4]]
  (let [coming      (calc/calculate-angle p2 p1)
        going       (calc/calculate-angle p4 p3)
        x-at-paddle (+ paddle-width ball-radius)
        y-at-paddle (calc/calculate-y-at-x coming p1 x-at-paddle)
        paddle-pos  (/ (+ (last p2) (last p3)) 2)
        paddle-ctr  (+ paddle-pos (/ paddle-height 2))]
    [coming going (- y-at-paddle paddle-ctr)]))

(defn turnangles 
  "Returns coming and leaving angles and averaged paddle hitpoint on each left player hit"
  [game]
  (map turnangle (turnpoints game)))

; order by coming going

(defn dump-results 
  []
  (let [logs    (.. (java.io.File. "log") listFiles )
        games   (map load-game logs)
        results (mapcat turnangles games)
        sorted  (sort-by (juxt first second) results)]
    (pprint sorted)))
    
  
        
