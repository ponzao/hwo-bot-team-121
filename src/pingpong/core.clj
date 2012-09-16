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

; contains data entries of the ongoing game
(def game-data (atom ()))

(def ball-events (atom ()))

; temporary
(def paddle-positions (atom ()))

(def last-sent-timestamp (atom (System/currentTimeMillis)))

(def last-direction (atom 0))

; strategy

;; ([:win? true/false :style :style1/style2/style3/style4/:aggressive/:defensive/:optimistic/:anticipating/:...], ...)
(def game-history (atom ()))

;; TODO based on game history
(defn calculate-playing-style []
  :basic)

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

(defn ball-target-calculator
  [max-width max-height paddle-height paddle-width ball-radius] 
  (let [x-at-paddle (+ paddle-width ball-radius)
        height      (- max-height ball-radius)]
    (fn [p1 p2] ; [[x1 y1] [x2 y2]]
      (loop [angle (calculate-angle p1 p2)           
             point p2]
        (let [y-at-paddle (calculate-y-at-x angle point x-at-paddle)
              out (out-of-bounds ball-radius height y-at-paddle)]
          (if out
            ; TODO: Simplify by returning points from calculate-...
            (recur (* -1 angle) [(calculate-x-at-y angle point (if (= out :over)
                                                                 height
                                                                 ball-radius))
                                 (if (= out :over) height ball-radius)])
            [x-at-paddle y-at-paddle]))))))

; paddle target calculation

; vertical increment for 1/10 second
(def paddle-speed (atom nil))

; FIXME this is not reliable
(defn calculate-paddle-speed []
  "initializes paddle-speed after first two moves"
  (when (and (nil? @paddle-speed)
             (= (count @game-data) 2))
    (let [[one two & _] @game-data
           distance    (- (-> one :left :y) (-> two :left :y))
           time-in-ms  (- (-> one :time) (-> two :time))
           speed       (/ distance (/ time-in-ms 100))]
      ; XXX for some reason calculated speed times two works better
      (reset! paddle-speed (* 2.0 speed)))))

; workaround
(defn init-paddle-speed! [data]
  (when-not @paddle-speed) 
    (reset! paddle-speed (* 0.375 (-> data :conf :paddleHeight))))

;; Nice to have?
(defn opponent-paddle-direction
  [?])

(defn ball-direction [[x1 _] [x2 _]]
  (if (< x1 x2) :left :right))

(defn paddle-destination-calculator
  [{left :left ball :ball {:keys [maxWidth maxHeight paddleHeight paddleWidth ballRadius]} :conf}]
  (let [ball-target-calc (ball-target-calculator maxWidth maxHeight 
                                                 paddleHeight paddleWidth
                                                 ballRadius)
        center-position  (- (/ maxHeight 2) (/ paddleHeight 2))
        max-position     (- maxHeight paddleHeight)]
    (fn [p1 p2] ; [[x1 y1] [x2 y2]]      
      (let [[_ ball-target] (ball-target-calc p1 p2)
            ball-dir        (ball-direction p1 p2)
            target          (- ball-target (/ paddleHeight 2))]
        (case ball-dir
          :left (constrain max-position target)
          :right center-position)))))

(def paddle-destination-calc (atom nil))

(defn write [conn data]
  (doto (:out @conn)
    (.println (json-str data))
    (.flush)))

(defn move-paddle!
  [conn direction]
  (when-not (= direction @last-direction)
    (reset! last-direction direction)
    (reset! last-sent-timestamp (System/currentTimeMillis))
    (write conn {:msgType "changeDir" :data direction})))

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
      
; TODO instead of two events the strategy should get the final movement vector of the ball

; hits ball with paddle center
(defn basic-strategy-move [conf position target event1 event2]
  (let [diff  (- target position)
        speed @paddle-speed]
    (if (<= (Math/abs diff) speed)
      (/ diff speed)
      (if (< diff 0) -1 1))))

; hits ball with paddle corner of ball direction
(defn corner-strategy-move [conf position target [x1 y1 :as ev1] [x2 y2 :as ev2]]
  (let [{:keys [maxWidth maxHeight paddleHeight paddleWidth ballRadius]} conf
        offset        (- (/ paddleHeight 2) ballRadius) 
        max-position  (- maxHeight paddleHeight)
        corner        (if (< y1 y2) ; XXX not reliable for reflections
                        (+ target offset)
                        (- target offset))
        new-target  (constrain max-position corner)]
    (basic-strategy-move conf position new-target ev1 ev2)))

; aims ball with paddle center, but moves into opposite ball direction when ball is near
(defn mobile-strategy-move [conf position target [x1 y1 :as ev1] [x2 y2 :as ev2]]
  (let [{:keys [maxWidth maxHeight paddleHeight paddleWidth ballRadius]} conf
        x-at-paddle (+ paddleWidth ballRadius)
        ball-dir    (if (< y1 y2) :up :down) ; XXX not reliable for reflections
        ball-speed  (- x2 x1)
        near       (< (- x1 x-at-paddle) (* 2 ball-speed))] 
    (if near 
      ; TODO don't do the move in corners
      (case ball-dir :up 1 :down -1)
      (basic-strategy-move conf position target ev1 ev2)))) 

(defn calculate-and-make-move! [conn data]
  (let [[event1 event2] (take-ball-events @ball-events) 
        position        (-> data :left :y)
        target          (@paddle-destination-calc event1 event2)
        diff            (- target position)
        speed            @paddle-speed
                        ; TODO strategy selection 
        movement        (corner-strategy-move (:conf data ) position target event1 event2)]
    (move-paddle! conn movement)))
                  
(defn time-diff []
  (let [current (System/currentTimeMillis)
        old @last-sent-timestamp]
    (> (- current old) 100)))

(defn init-paddle-dest-calc! [data]
  (when (nil? @paddle-destination-calc) 
    (reset! paddle-destination-calc (paddle-destination-calculator data))))

(defn make-move [conn {{{ball-x :x ball-y :y} :pos} :ball
                       {paddle-y :y} :left :as data}]
  (swap! ball-events conj [ball-x ball-y])  
  (swap! paddle-positions conj paddle-y) ; XXX temporary 
  (init-paddle-speed! data)
  (init-paddle-dest-calc! data)    
  (when (time-diff) ; only react max 10 times / sec
    (swap! game-data conj data)    
    (when (and (first @ball-events) (second @ball-events)) ; react after 2 ball events      
      (calculate-and-make-move! conn data))))
      
; game control

(def url (atom nil))

(def conn (atom nil))

(defn handle-message [conn {msgType :msgType data :data}]
  (case msgType
    joined (do (info (str "Game joined successfully. Use following URL for visualization: " data))
               (reset! url data))
    gameStarted (info (str "Game started: " (nth data 0) " vs. " (nth data 1)))
    gameIsOn (make-move conn data)
    gameIsOver (do (info (str "Game ended. Winner: " data))
                   (reset! game-data ()))
    error (error data)
    'pass))

(defn parse-message [data]
  (try
    (let [msg (read-json data)]
      {:msgType (symbol (:msgType msg))
       :data    (:data msg)})
    (catch Throwable e {:msgType 'error :data (. e getMessage)})))

(defn conn-handler []
  (while (nil? (:exit @conn))
    (let [msg (.readLine (:in @conn))]
      (cond
       (nil? msg) (dosync (swap! conn assoc :exit true))
       :else (handle-message conn (parse-message msg))))))

(defn connect [server]
  (let [socket (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))]
    (reset! conn {:in in :out out})
    (doto (Thread. conn-handler) (.start))
    conn))

(defn -main [team-name hostname port]
  (let [s (connect {:name hostname :port (read-string port)})
        join-message {:msgType "join" :data team-name}]
    (write s join-message)))

(comment (-main "mysema" "boris.helloworldopen.fi" "9090"))


