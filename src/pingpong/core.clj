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

(def ball-events (atom ()))

; temporary
(def last-sent-timestamp (atom (System/currentTimeMillis)))

(def last-direction (atom 0))

; strategy

;; ({:win? true/false :style :style1/style2/style3/style4/:aggressive/:defensive/:optimistic/:anticipating/:...}, ...)
(def game-history (atom ()))

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

(defn calculate-paddle-speed [data]
  (* 0.375 (-> data :conf :paddleHeight)))

(defn ball-direction [[x1 _] [x2 _]]
  (if (< x1 x2) :left :right))

; XXX can we handle this logic somehow in the strategies?
(defn calculate-paddle-target
  [{left :left ball :ball
    {:keys [maxWidth maxHeight paddleHeight paddleWidth ballRadius] :as conf} :conf
    :as data} p1 p2]
  (let [center-position  (- (/ maxHeight 2) (/ paddleHeight 2))
        max-position     (- maxHeight paddleHeight)
        [angle _ ball-target] (calculate-ball-target conf p1 p2)
        ball-dir        (ball-direction p1 p2)
        target          (case ball-dir
                          :left (- ball-target (/ paddleHeight 2))
                          :right center-position)]
    [angle ball-target target]))

(defn write! [conn data]
  (doto (:out conn)
    (.println (json-str data))
    (.flush)))

(defn move-paddle!
  [conn direction]
  (when-not (= direction @last-direction)
    (reset! last-direction direction)
    (reset! last-sent-timestamp (System/currentTimeMillis))
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
      
; hits ball at calculated position (paddle center)
(defn basic-strategy-move [data {:keys [maxHeight paddleHeight]} position angle target]
  (let [max-position (- maxHeight paddleHeight)
        norm-target  (constrain max-position target)    
        diff         (- norm-target position)
        speed        (calculate-paddle-speed data)]
    (if (<= (Math/abs diff) speed)
      (/ diff speed)
      (if (< diff 0) -1 1))))

; hits ball with paddle corner of ball direction
(defn corner-strategy-move [data conf position angle target]
  (let [{:keys [maxHeight paddleHeight ballRadius]} conf
        offset        (- (/ paddleHeight 2) ballRadius)
        max-position  (- maxHeight paddleHeight)
        corner        ((if (neg? angle) - +) target offset)
        new-target  (constrain max-position corner)]
    (basic-strategy-move data conf position angle new-target)))

; TODO strategies with movement etc
(defn calculate-move [data]
  (let [[event1 event2] (take-ball-events @ball-events) 
        position        (-> data :left :y)
        [angle ball target] (calculate-paddle-target data event1 event2)
        diff            (- target position)
        speed            (calculate-paddle-speed data)
                        ; TODO strategy selection 
        movement        (corner-strategy-move data (:conf data) position angle target)]
    movement))
                  
(defn time-diff []
  (let [current (System/currentTimeMillis)
        old @last-sent-timestamp]
    (> (- current old) 100)))

(defn make-move! [conn {{{ball-x :x ball-y :y} :pos} :ball
                        {paddle-y :y} :left :as data}]
  (swap! ball-events conj [ball-x ball-y])  
  (when (time-diff) ; only react max 10 times / sec
    (when (and (first @ball-events) (second @ball-events)) ; react after 2 ball events      
      (move-paddle! conn (calculate-move data)))))
      
; game control

(def current-game-data (atom []))
(def lost-games (atom []))

(defn handle-message [conn {msg-type :msgType data :data}]
  (case msg-type
    :joined (println (str "Game joined successfully. Use following URL for visualization: " data))
    :gameStarted (println (str "Game started: " (first data) " vs. " (second data)))
    :gameIsOn (do (swap! current-game-data conj data)
                  (make-move! conn data))
    :gameIsOver (do (println (str "Game ended. Winner: " data))
                    (when (not= data "mysema")
                      (swap! lost-games conj @current-game-data))
                    (reset! current-game-data [])
                    (let [{win-count true
                           lose-count false} (frequencies (map :win? (swap! game-history conj {:win? (= "mysema" data)})))]
                      (println win-count "-" lose-count)))
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
  (do (doseq [msg (game-data-seq conn)]
        (handle-message conn (parse-message msg)))
      (stop conn)))

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

(comment (use '(incanter core stats charts))

         (defn view-ball
           [games]
           (doseq [game games]
             (let [pos (comp :pos :ball)
                   ball-x (map (comp :x pos) game)
                   ball-y (map (comp :y pos) game)]
               (view (xy-plot ball-x ball-y))))))