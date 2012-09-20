(ns pingpong.strategies
  (require [pingpong.calc :as calc]))

(defn- ball-moves-right
  "Calculates optimal paddle movement when ball moves right"
  [conf paddle-position ball-angle ball-target]
  (let [{:keys [maxWidth paddleWidth ballRadius]} conf
        start      [(- maxWidth paddleWidth ballRadius) ball-target]
        back-angle (* -1 ball-angle)
        step-x     (- (first start) ballRadius)
        step-y     (calc/calculate-y-at-x back-angle start step-x)
        back-y     (nth (calc/calculate-ball-target conf :left [step-x step-y] start) 2)]
    back-y))
        
(defn basic
  "Hits ball at calculated position (paddle's center)."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight]} conf
        target (case ball-dir
                 :left  (- ball-target (/ paddleHeight 2))
                 :right (ball-moves-right conf paddle-position ball-angle ball-target))]
                 ;:right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (calc/approach-target conf paddle-position target)))

(defn accelerating
  "Moves the paddle just before hitting."
  [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        area-center (- (/ maxHeight 2) (/ paddleHeight 2))
        paddle-center (- ball-target (/ paddleHeight 2))
        offset (- (/ paddleHeight 2) ballRadius)
        target (case ball-dir
                 :left (if (< toimpact 400) 
                         ((if (neg? ball-angle) - +) paddle-center offset) 
                         paddle-center)
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
                   (if (neg? ball-angle) (- maxHeight ballRadius) ballRadius)]
        off-angle (calc/calculate-angle opposite
                                   [(+ paddleWidth ballRadius) ball-target])
        center    (- ball-target (/ paddleHeight 2))         
        offset    (* (calc/constrain -1 1 (* 2.5 (+ off-angle ball-angle)))         
                     (- (/ paddleHeight 2) ballRadius) )
        target   (case ball-dir 
                   :left  (- center offset)
                   :right (ball-moves-right conf paddle-position ball-angle ball-target))]
                   ;:right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (calc/approach-target conf paddle-position target)))
