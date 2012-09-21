(ns pingpong.calc)

(defn calculate-angle
  "Calculates angle between two points."
  [[x1 y1] [x2 y2]]
  (if (= x2 x1) 0
    (/ (- y2 y1) (- x2 x1))))

(defn calculate-y-at-x
  "Calculates y based on x, angle and a
   point."
  [angle [x1 y1] x2]
  (+ (* (- x2 x1) angle) y1))

(defn calculate-x-at-y
  "Calculates x based on y, angle and a
   point."
  [angle [x1 y1] y2]
  (if-not (zero? angle)
    (+ (/ (- y2 y1) angle) x1)
    x1))

(defn out-of-bounds
  "Returns :under or :over if point is
   under or over the given boundaries."
  ([min max n]
    (cond (< n min) :under
          (> n max) :over))
  ([max n] (out-of-bounds 0 max n)))

(defn constrain
  ;TODO: Document!
  ([min max x]
    (cond (< x min) min
          (> x max) max
          :else     x))
  ([max x] (constrain 0 max x)))

(defn time-to-target
  ;TODO: Document!
  [direction {:keys [maxWidth paddleWidth]}
   [x1 y1 t1] [x2 y2 t2]]
  (case direction 
    :left (let [dist (- x1 x2)
                time (- t2 t1)
                speed (/ dist time)]
            (/ x2 speed))
    :right 100000000))
  
(defn calculate-ball-target
  "Calculates ball target."
  [{:keys [maxWidth maxHeight paddleWidth ballRadius]} side p1 p2]
  (let [x-at-paddle (case side
                      :left  (+ paddleWidth ballRadius)
                      :right (- maxWidth paddleWidth ballRadius))
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

(defn calculate-paddle-speed
  "Calculates paddle speed."
  [conf]
  (* 0.375 (:paddleHeight conf)))

(defn ball-direction
  "Based on two points returns :left if ball
   is moving left and :right if moving right."
  [[x1 _] [x2 _]]
  (if (< x1 x2) :left :right))

(defn get-segment [position maxHeight]
  ; TODO: Document!
  (cond (< position (* 0.2 maxHeight))  1
        (< position (* 0.8 maxHeight))  2
        :else 3))

(defn approach-target
  ;TODO: Document!
  [{:keys [maxHeight paddleHeight] :as conf} position target]
  (let [max-position (- maxHeight paddleHeight)
        norm-target  (constrain max-position target)
        diff         (- norm-target position)
        speed        (calculate-paddle-speed conf)]        
    (if (<= (Math/abs diff) speed)
      (/ diff speed)
      (if (neg? diff) -1 1))))
