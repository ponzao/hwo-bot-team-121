(ns pingpong.strategies
  (require [pingpong.calc :as calc]))

(defn basic
  "Hits ball at calculated position (paddle's center)."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight]} conf
        target (case ball-dir
                 :left  (- ball-target (/ paddleHeight 2))
                 :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (calc/approach-target conf paddle-position target)))

(defn accelerating
  "Moves the paddle just before hitting."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight]} conf
        area-center (- (/ maxHeight 2) (/ paddleHeight 2))
        paddle-center (- ball-target (/ paddleHeight 2))     
        target (case ball-dir
                 :left (if (> 400 toimpact) (if (neg? ball-angle) maxHeight 0) paddle-center)
                 :right area-center)]
    (calc/approach-target conf paddle-position target)))


(defn zigzag
  "Hits ball with paddle's corner."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (neg? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (case ball-dir 
                 :left  (+ ball-target offset)
                 :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (calc/approach-target conf paddle-position target)))

(defn corner
  "Hits ball into corners."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight maxWidth paddleWidth paddleHeight ballRadius]} conf
        opposite  [(- maxWidth ballRadius)
                   (if (neg? ball-angle) ballRadius (- maxHeight ballRadius))]
        off-angle (calc/calculate-angle opposite
                                   [(+ paddleWidth ballRadius) ball-target])
        center    (- ball-target (/ paddleHeight 2))         
        offset    (* (calc/constrain -1 1 (- off-angle ball-angle))         
                     (- (/ paddleHeight 2) ballRadius) )
        target   (case ball-dir 
                   :left  (+ center offset)
                   :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    ;(when (= ball-dir :left)
    ;  (println ball-angle ">" off-angle))
    (calc/approach-target conf paddle-position target)))
