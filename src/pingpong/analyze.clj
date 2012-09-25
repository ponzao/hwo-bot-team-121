(ns pingpong.analyze
  (require [pingpong.calc :as calc]))

(defn loadgame [file] 
  (drop 2 (read-string (slurp file))))

(defn bx [m] (get-in m [:ball :pos :x]))
(defn by [m] (get-in m [:ball :pos :y]))
(defn ly [m] (get-in m [:left :y]))
(defn ry [m] (get-in m [:right :y]))

;TODO Check is the padhit right to calc like this?
(defn x-y-padhit [game] 
  (map (juxt bx by #(- (by %) (ly %))) game))

(defn split-chunks [n s]
  (when (and (seq s) (> (count s) (dec n)))
    (cons (take n s) (split-chunks n (rest s)))))

(defn turnpoint? 
  [[x1 x2 x3 x4 :as pts]]
  (and (> x1 x2) (> x1 x3) (> x4 x3) (> x4 x2) pts))

(defn turnpoints [game]
  "Returns four [x y paddlehit] positions for each left player turnpoints"
  (filter #(turnpoint? (map first %)) (split-chunks 4 (x-y-padhit game))))

(defn turnangles [game]
  "Returns coming and leaving angles and averaged paddle hitpoint on each left player hit"
  (map 
    (fn [[p1 p2 p3 p4]]
      [(calc/calculate-angle p1 p2)
       (calc/calculate-angle p2 p4)
       (/ (+ (last p2) (last p3)) 2)]) 
    (turnpoints game)))