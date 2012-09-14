(ns pingpong.core
  (:use [clojure.data.json :only (read-json json-str)]
        [clojure.tools.logging :only (info error)]
        [clojure.string :only (join)])
  (:import [java.net Socket]
           [java.io PrintWriter InputStreamReader BufferedReader])
  (:gen-class :main true))

;; ([:win? true/false :style :style1/style2/style3/style4/:aggressive/:defensive/:optimistic/:anticipating/:...], ...)
(def game-history (atom ()))

;; based on game history
(defn calculate-playing-style
  []
  :basic)

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

;; TODO: 'ball-target-calculator'
(defn trajectory-calculator
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

;; atom or bindable var
(def *paddle-speed*)

;; Calculated after first move
;; Called somewhere(?)
(defn calculate-paddle-speed
  [state1 state2])

;; Nice to have?
(defn opponent-paddle-direction
  [?])

;; TODO: Use trajectory-calculator only inside this
;; [x y] [:left :up] -> [x y]
(defn paddle-destination-calculator
  [ball-destination ball-direction])

;; , -1..1
(defn move-paddle!
  [conn direction])

;; Calls move-paddle!, paddle-destination-calculator...
(defn calculate-and-make-move!
  [conn lots-of-data])

;; :right/:left
(defn ball-direction
  [?])

;; TODO: seq of events(?)

(def ball-events (atom ()))
(def last-sent-timestamp (atom (System/currentTimeMillis)))

(def stay {:msgType "changeDir" :data 0.0})

(def move-up {:msgType "changeDir" :data -1.0})

(def move-down {:msgType "changeDir" :data 1.0})

;; TODO: No need to resend same event
(defn time-diff
  []
  (let [current (System/currentTimeMillis)
        old @last-sent-timestamp]
    (> (- current old) 100)))

(defn write [conn data]
  (doto (:out @conn)
    (.println (json-str data))
    (.flush)))

(def guesstimate (trajectory-calculator 640 480 nil nil nil))

(defn moving-right?
  [[x1 _] [x2 _]]
  (> x1 x2))

(defn near?
  [x1 x2]
  (< (Math/abs (- x1 x2)) 10))

(defn make-move [conn {{{ball-x :x ball-y :y} :pos} :ball
                       {paddle-y :y} :left :as data}]
  (swap! ball-events conj [ball-x ball-y])
  (when (time-diff)
    (reset! last-sent-timestamp (System/currentTimeMillis))
    (when (and (first @ball-events) (second @ball-events))
      (let [[event1 event2] (take 2 @ball-events)
            [_ expected-y] (if (moving-right? event1 event2)
                             [nil 240]
                             (guesstimate event1 event2))
            paddle-mid (+ paddle-y 25)]
        (write conn (cond 
                          (< (Math/abs (- expected-y paddle-mid)) 10) stay
                          (< expected-y paddle-mid) move-up
                          :else move-down))))))

(def url (atom nil))
(def conn (atom nil))

(defn handle-message [conn {msgType :msgType data :data}]
  (case msgType
    joined (reset! url data)
    gameStarted (info (str "Game started: " (nth data 0) " vs. " (nth data 1)))
    gameIsOn (make-move conn data)
    gameIsOver (info (str "Game ended. Winner: " data))
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


