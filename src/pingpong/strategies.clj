(ns pingpong.strategies
  (require [pingpong.calc :as calc]))

(defn- ball-moves-right
  "Calculates optimal paddle movement when ball moves right"
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxWidth paddleWidth ballRadius]} conf
        start      [(- maxWidth paddleWidth ballRadius) ball-target]
        back-angle (* -1 ball-angle)
        step-x     (- (first start) ballRadius)
        step-y     (calc/calculate-y-at-x back-angle start step-x)
        back-y     (nth (calc/calculate-ball-target conf :left [step-x step-y] start) 2)]
    back-y))
        
(defn basic
  "Hits ball at calculated position (paddle's center)."
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight]} conf
        target (- ball-target (/ paddleHeight 2))]
    target))

(defn accelerating
  "Moves the paddle just before hitting."
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        area-center (- (/ maxHeight 2) (/ paddleHeight 2))
        paddle-center (- ball-target (/ paddleHeight 2))
        offset (- (/ paddleHeight 2) ballRadius)
        target (if (< toimpact 400) 
                 ((if (neg? ball-angle) - +) paddle-center offset) 
                 paddle-center)]
    target))

(defn zigzag
  "Hits ball with paddle's corner."
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (neg? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (+ ball-target offset)]
    target))

(defn corner
  "Hits ball into corners."
  [conf position ball-angle ball-dir ball-target toimpact]
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

(defn anti-corner
  "Opposite action of zigzag. Good strategy against corner."
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (pos? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (+ ball-target offset)]
    target))

; deprecated, improve corner strategy instead
(defn combo
  "Uses accelerating for small ball angles and corner for bigger ones"
  [conf position ball-angle ball-dir ball-target toimpact]
  (let [strategy (if (< (Math/abs ball-angle) 0.2) 
                   accelerating 
                   corner)]
    (strategy conf position ball-angle ball-dir ball-target toimpact)))

(defn create-strategy [strategy]
  (fn [& [conf position ball-angle ball-dir & _ :as args]]
    (let [target (case ball-dir
                   :left  (apply strategy args)
                   :right (apply ball-moves-right args))]
      (calc/approach-target conf position target))))

(def all 
  (->> [:basic :accelerating :zigzag :corner :anti-corner :combo]
       (map #(vector % (-> % name symbol resolve create-strategy)))
       (into {})))     
   
