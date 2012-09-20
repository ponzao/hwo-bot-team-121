(ns pingpong.calc)

(defn calculate-angle
  [[x1 y1] [x2 y2]]
  (if (= x2 x1) 0
    (/ (- y2 y1) (- x2 x1))))

(defn calculate-y-at-x
  [angle [x1 y1] x2]
  (+ (* (- x2 x1) angle) y1))

(defn calculate-x-at-y
  [angle [x1 y1] y2]
  (if-not (zero? angle)
    (+ (/ (- y2 y1) angle) x1)
    x1))

(defn out-of-bounds
  ([min max x]
    (cond (< x min) :under
          (> x max) :over))
  ([max x] (out-of-bounds 0 max x)))

(defn constrain
  ([min max x]
    (cond (< x min) min
          (> x max) max
          :else     x))
  ([max x] (constrain 0 max x)))

(defn time-left-to-hit-target
  [direction {:keys [maxWidth paddleWidth]} [x1 y1 t1] [x2 y2 t2]]
  (case direction 
    :left (let [dist (- x1 x2)
                time (- t2 t1)
                speed (/ dist time)]
            (/ x2 speed))
    :right 100000000))

(defn calculate-ball-target
  [{:keys [maxWidth maxHeight paddleHeight paddleWidth ballRadius]} p1 p2]
  (let [x-at-paddle (+ paddleWidth ballRadius)
        height      (- maxHeight ballRadius)]
    (loop [angle (calculate-angle p1 p2)     
           point p2]
      (let [y-at-paddle (calculate-y-at-x angle point x-at-paddle)
            out (out-of-bounds ballRadius height y-at-paddle)]
        (if out
          (recur (* -1 angle) [(calculate-x-at-y angle point (if (= out :over)
                                                               height
                                                               ballRadius))
                               (if (= out :over) height ballRadius)])
          [angle x-at-paddle y-at-paddle])))))

(defn calculate-paddle-speed [conf]
  (* 0.375 (:paddleHeight conf)))

(defn ball-direction [[x1 _] [x2 _]]
  (if (< x1 x2) :left :right))

(defn approach-target [{:keys [maxHeight paddleHeight] :as conf} position target]
  (let [max-position (- maxHeight paddleHeight)
        norm-target  (constrain max-position target)
        diff         (- norm-target position)
        speed        (calculate-paddle-speed conf)]        
    (if (<= (Math/abs diff) speed)
      (/ diff speed)
      (if (neg? diff) -1 1))))
