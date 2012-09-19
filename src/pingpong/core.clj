(ns pingpong.core
  (:use [clojure.data.json :only (read-json json-str)]
        [clojure.tools.logging :only (info error)]
        [clojure.string :only (join)])
  (:import [java.net Socket]
           [java.io PrintWriter InputStreamReader BufferedReader])
  (:gen-class :main true))

; ball always between x  (+ paddle-width ball-radius), (- max-width ball-radius) 
;                     y  ball-radius, (- max-height ball-radius)

; paddle always between y 0, (- max-height paddle-height)

; ball target calculation
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

(defn ball-speed 
  [x1 t1 x2 t2]
  (if (= t1 t2) 0 (Math/abs (double (/ (- x2 x1) (- t2 t1))))))

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

(defn write! [conn data]
  (doto (:out conn)
    (.println (json-str data))
    (.flush)))

(defn move-paddle!
  [conn direction last-direction last-timestamp]
  (when-not (= direction @last-direction)
    (reset! last-direction direction)
    (reset! last-timestamp (System/currentTimeMillis))
    (write! conn {:msgType "changeDir" :data direction})))

(defn take-ball-events [[event1 event2 & _ :as events]]
  (if (> (count events) 2)
    (let [event3 (nth events 2)
          angle1 (Math/abs (calculate-angle event1 event2))
          angle2 (Math/abs (calculate-angle event2 event3))
          diff   (Math/abs (- angle1 angle2))]
      (if (< diff 0.01)
        [event1 event2]
        (take-ball-events (rest events))))
    [event1 event2]))
      
(defn approach-target [{:keys [maxHeight paddleHeight] :as conf} position target]
  (let [max-position (- maxHeight paddleHeight)
        norm-target  (constrain max-position target)
        diff         (- norm-target position)
        speed        (calculate-paddle-speed conf)]        
    (if (<= (Math/abs diff) speed)
      (/ diff speed)
      (if (neg? diff) -1 1))))

; hits ball at calculated position (paddle center)
(defn basic-strategy-move [conf paddle-position ball-angle ball-dir ball-target]
  (let [{:keys [maxHeight paddleHeight]} conf
        target (case ball-dir
                 :left  (- ball-target (/ paddleHeight 2))
                 :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (approach-target conf paddle-position target)))

;moves the paddle just before hitting
(defn speeding-basic-strategy-move [conf paddle-position ball-angle ball-dir ball-target toimpact]
  (let [{:keys [maxHeight paddleHeight]} conf
        area-center (- (/ maxHeight 2) (/ paddleHeight 2))
        paddle-center (- ball-target (/ paddleHeight 2))
        
        target (case ball-dir
                 :left (if (> 400 toimpact) (if (neg? ball-angle) maxHeight 0) paddle-center)
                 :right area-center)
        ]
    (approach-target conf paddle-position target)))

; hits ball with paddle corner of ball direction
(defn paddle-corner-strategy-move [conf paddle-position ball-angle ball-dir ball-target]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset (if (neg? ball-angle) (- ballRadius paddleHeight)
                                     (* -1 ballRadius)) 
        target (case ball-dir 
                 :left  (+ ball-target offset)
                 :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    (approach-target conf paddle-position target)))

; aims to position ball in opposite corner
(defn corner-strategy-move [conf paddle-position ball-angle ball-dir ball-target]
  (let [{:keys [maxHeight maxWidth paddleWidth paddleHeight ballRadius]} conf
        opposite  [(- maxWidth ballRadius)
                   (if (neg? ball-angle) ballRadius (- maxHeight ballRadius))]
        off-angle (calculate-angle opposite
                                   [(+ paddleWidth ballRadius) ball-target])
        center    (- ball-target (/ paddleHeight 2))         
        offset    (* (constrain -1 1 (- off-angle ball-angle))         
                     (- (/ paddleHeight 2) ballRadius) )
        target   (case ball-dir 
                   :left  (+ center offset)
                   :right (- (/ maxHeight 2) (/ paddleHeight 2)))]
    ;(when (= ball-dir :left)
    ;  (println ball-angle ">" off-angle))
    (approach-target conf paddle-position target)))

; TODO strategies with movement etc

;
(defn calculate-move [data ball-events]
  (let [[event1 event2]  (take-ball-events ball-events) 
        position         (-> data :left :y)
        [angle _ target] (calculate-ball-target (:conf data) event1 event2)
        direction        (ball-direction event1 event2)
        toimpact         (time-left-to-hit-target direction (:conf data) event2 event1)
        movement         (speeding-basic-strategy-move (:conf data) position angle direction target toimpact)]
    movement))

(defn time-diff [last-timestamp]
  (let [current (System/currentTimeMillis)
        old @last-timestamp]
    (> (- current old) 100)))

(defn make-move! [conn {{{ball-x :x ball-y :y} :pos} :ball
                        {paddle-y :y} :left :as data} ball-events last-timestamp last-direction]
  (swap! ball-events conj [ball-x ball-y (:time data)])
  (when (time-diff last-timestamp) ; only react max 10 times / sec
    (when (and (first @ball-events) (second @ball-events)) ; react after 2 ball events      
      (move-paddle! conn (calculate-move data @ball-events) last-direction last-timestamp))))
      
(defn handle-message [conn {msg-type :msgType data :data} ball-events game-history last-timestamp
                      last-direction]
  (case msg-type
    :joined (println (str "Game joined successfully. Use following URL for visualization: " data))
    :gameStarted (println (str "Game started: " (first data) " vs. " (second data)))
    :gameIsOn (make-move! conn data ball-events last-timestamp last-direction)
    :gameIsOver (do (println (str "Game ended. Winner: " data))
                    (reset! ball-events ()))
    :error (println "error: " data)
    :pass))

(defn parse-message [data]
  (try
    (update-in (read-json data true) [:msgType] keyword)
    (catch Throwable e {:msg-type :error :data (. e getMessage)})))

(defn stop [conn] (assoc conn :exit true)) 
                          
(defn read-msg
  [conn]
  (.readLine (:in conn)))

(def not-nil? (complement nil?))

(defn game-data-seq
  [conn]
  (take-while not-nil? (repeatedly #(read-msg conn))))

(defn conn-handler [conn]
  (let [game-history (atom [])
        ball-events (atom ())
        last-direction (atom nil)
        last-timestamp (atom (System/currentTimeMillis))]
    (do (doseq [msg (game-data-seq conn)]
          (handle-message conn (parse-message msg)
                          ball-events game-history
                          last-timestamp last-direction))
        (stop conn))))

(defn connect [server]
  (let [socket (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))]
    {:in in :out out}))

(defn -main [team-name hostname port]
  (let [conn (connect {:name hostname :port (read-string port)})
        join-message {:msgType "join" :data team-name}]
    (write! conn join-message)
    (.start (Thread. #(conn-handler conn)))))

(defn start [] (-main "mysema" "boris.helloworldopen.fi" "9090"))

(defn start-duel [team1 team2]
  (let [conn (connect {:name "boris.helloworldopen.fi" :port 9090})
        join-message {:msgType "requestDuel" :data [team1 team2]}]
    (write! conn join-message)
    (.start (Thread. #(conn-handler conn)))))

(comment
(use '[incanter core stats charts])

(defn view-games
  [games]
  (doseq [game games]
    (let [pos (comp :pos :ball)
          ball-x (map (comp :x pos) game)
          ball-y (map (comp :y pos) game)
          left-x (iterate (partial + 0.2) 0)
          left-y (map (comp :y :left) game)
          right-x (iterate #(- % 0.2) 640)
          right-y (map (comp :y :right) game)]
      (view (doto (scatter-plot ball-x ball-y)
              (set-x-range 0 640)
              (set-y-range 0 480)
              (add-points left-x left-y)
              (add-points right-x right-y)))))))
