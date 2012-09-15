(ns pingpong.core
  (:use [clojure.data.json :only (read-json json-str)]
        [clojure.tools.logging :only (info error)]
        [clojure.string :only (join)])
  (:import [java.net Socket]
           [java.io PrintWriter InputStreamReader BufferedReader])
  (:gen-class :main true))

; contains data entries of the ongoing game
(def game-data (atom ()))

(def ball-events (atom ()))

(def last-sent-timestamp (atom (System/currentTimeMillis)))

; strategy

;; ([:win? true/false :style :style1/style2/style3/style4/:aggressive/:defensive/:optimistic/:anticipating/:...], ...)
(def game-history (atom ()))

;; TODO based on game history
(defn calculate-playing-style []
  :basic)

; ball target calculation

(defn calculate-angle
  [[x1 y1] [x2 y2]]
  (/ (- y2 y1) (- x2 x1)))

(defn calculate-y-at-x
  [angle [x1 y1] x2]
  (+ (* (- x2 x1) angle) y1))

(defn calculate-x-at-y
  [angle [x1 y1] y2]
  (+ (/ (- y2 y1) angle) x1))

(defn out-of-bounds
  [height y]
  (cond (neg? y) :under 
        (< height y) :over))

(defn ball-target-calculator
  [width height paddle-height paddle-width ball-radius]
  (fn [p1 p2]
    (loop [angle (calculate-angle p1 p2)
           point p2]
      (let [y-at-zero (calculate-y-at-x angle point 0)
            out (out-of-bounds height y-at-zero)]
        (if out
          ; TODO: Simplify by returning points from calculate-...
          (recur (* -1 angle) [(calculate-x-at-y angle point (if (= out :over)
                                                               height
                                                               0))
                               (if (= out :over) height 0)])
          [0 y-at-zero])))))

; paddle target calculation

(def paddle-speed (atom nil))

; FIXME
(defn calculate-paddle-speed []
  "initializes paddle-speed after first two moves"
  (when (and (nil? @paddle-speed)
             (= (count @game-data) 3)))
    (let [one (first @game-data)
          two (second @game-data)]
      (reset! paddle-speed (- (-> two :left :y) (-> one :left :y)))))

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
        center-position  (- (/ maxHeight 2) (/ paddleHeight 2))]
    (fn [p1 p2] ; [[x1 y1] [x2 y2]]      
      (let [[_ ball-target] (ball-target-calc p1 p2)
            ball-dir        (ball-direction p1 p2)]
        (case ball-dir
          :left  (- ball-target (/ paddleHeight 2))
          :right center-position)))))

(def paddle-destination-calc (atom nil))

(defn write [conn data]
  (doto (:out @conn)
    (.println (json-str data))
    (.flush)))

(defn move-paddle!
  [conn direction]
  (write conn {:msgType "changeDir" :data direction}))

(defn near? [x1 x2]
  (< (Math/abs (- x1 x2)) 10))

(defn calculate-and-make-move! [conn data]
  (let [[event1 event2] (take 2 @ball-events)
        paddle-position (-> data :left :y)
        paddle-target   (@paddle-destination-calc event1 event2)
        paddle-dir      (if (< paddle-position paddle-target) 1 -1)]
    (move-paddle! conn paddle-dir))) 
                  
(defn time-diff []
  (let [current (System/currentTimeMillis)
        old @last-sent-timestamp]
    (> (- current old) 100)))

; XXX
(defn init-paddle-dest-calc! [data]
  (when (nil? @paddle-destination-calc) 
    (reset! paddle-destination-calc (paddle-destination-calculator data))))

(defn make-move [conn {{{ball-x :x ball-y :y} :pos} :ball
                       {paddle-y :y} :left :as data}]    
  (swap! ball-events conj [ball-x ball-y])  
  (init-paddle-dest-calc! data)  
  (when (time-diff) ; only react max 10 times / sec
    (swap! game-data conj data)
    ;(calculate-paddle-speed)    
    (when (and (first @ball-events) (second @ball-events)) ; react after 2 ball events
      (reset! last-sent-timestamp (System/currentTimeMillis))
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


