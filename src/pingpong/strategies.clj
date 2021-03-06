(ns pingpong.strategies
  (require [pingpong.calc :as calc]))

(defn ball-moves-right
  "Calculates optimal paddle movement when ball moves right."
  [strategy conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxWidth paddleWidth paddleHeight ballRadius]} conf
        start      [(- maxWidth paddleWidth ballRadius) ball-target]
        back-angle (* -1 ball-angle)
        step-x     (- (first start) ballRadius)
        step-y     (calc/calculate-y-at-x back-angle start step-x)
        [angle back-x back-y] (calc/calculate-ball-target conf :left [step-x step-y] start)
        target     (strategy conf position angle :left back-y _)]
        ;target     (- back-y (/ paddleHeight 2))] 
    target))
        
(defn basic
  "Hits ball at calculated position (paddle's center)."
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight paddleHeight]} conf
        target (- ball-target (/ paddleHeight 2))]
    target))

(defn accelerating
  "Moves the paddle just before hitting."
  [conf position ball-angle ball-dir ball-target impact-time]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        area-center (- (/ maxHeight 2) (/ paddleHeight 2))
        paddle-center (- ball-target (/ paddleHeight 2))
        offset (- (/ paddleHeight 2) ballRadius)
        target (if (< impact-time 400) 
                 ((if (neg? ball-angle) - +) paddle-center offset) 
                 paddle-center)]
    target))

(defn zigzag
  "Hits ball with paddle's corner."
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (neg? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (+ ball-target offset)]
    target))

; deprecated
(defn corner-old
  "Hits ball into corners."
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight maxWidth paddleWidth paddleHeight ballRadius]} conf
        opposite  [(- maxWidth ballRadius)
                   (if (neg? ball-angle) (- maxHeight ballRadius) ballRadius)]
        off-angle (calc/calculate-angle opposite
                                   [(+ paddleWidth ballRadius) ball-target])
        center    (- ball-target (/ paddleHeight 2))         
        offset    (* (calc/constrain -1 1 (* 2.5 (+ off-angle ball-angle)))         
                     (- (/ paddleHeight 2) ballRadius) )
        target   (- center offset)]
    target))

(defn corner
  "Hits ball into corners."
  [conf position ball-angle ball-dir ball-target _]
(let [{:keys [maxHeight maxWidth paddleWidth paddleHeight ballRadius]} conf
        opposite  [(- maxWidth ballRadius)
                   (if (neg? ball-angle) (- maxHeight ballRadius) ballRadius)]
        off-angle (calc/calculate-angle opposite
                                   [(+ paddleWidth ballRadius) ball-target])
        center    (- ball-target (/ paddleHeight 2))         
        offset    (* (Math/sin (* Math/PI 1.6 (+  off-angle ball-angle)))         
                     (- (/ paddleHeight 2) ballRadius))
        target   (- center offset)]
    target))

(defn anti-corner
  "Opposite action of zigzag. Good strategy against corner."
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (pos? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (+ ball-target offset)]
    target))

; deprecated
(defn combo-old
  "Combines multiple strategies"
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        center-pos (or (> ball-target (* maxHeight 0.1))
                       (< ball-target (* maxHeight 0.9)))
        small-angle (< (Math/abs ball-angle) 0.1)
        strategy (if small-angle
                   (if center-pos anti-corner anti-corner)
                   (if center-pos corner-old anti-corner))]
    (strategy conf position ball-angle ball-dir ball-target _)))

(defn combo
  "Combines multiple strategies"
  [conf position ball-angle ball-dir ball-target _]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        center-pos (or (> ball-target (* maxHeight 0.1))
                       (< ball-target (* maxHeight 0.9)))
        small-angle (< (Math/abs ball-angle) 0.1)
        strategy (if small-angle
                   (if center-pos anti-corner anti-corner)
                   (if center-pos corner anti-corner))]
    (strategy conf position ball-angle ball-dir ball-target _)))

(defn create-strategy
  [strategy]
  (fn [& [conf position ball-angle ball-dir :as args]]
    (let [target (case ball-dir
                   :left  (apply strategy args)
                   :right (apply ball-moves-right (cons strategy args)))]
      (calc/approach-target conf position target))))

(def all 
  (->> [:basic :accelerating :zigzag :corner-old :corner :anti-corner :combo-old :combo]
       (map #(vector % (-> % name symbol resolve create-strategy)))
       (into {})))     
   
