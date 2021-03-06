(ns pingpong.core
  (:use [clojure.data.json :only (read-json json-str)]
        [clojure.tools.logging :only (info error)]
        [clojure.string :only (join)])
  (:require [pingpong.calc :as calc]
            [pingpong.strategies :as strategies])
  (:import [java.net Socket]
           [java.io File PrintWriter InputStreamReader BufferedReader])
  (:gen-class :main true))

(defn write!
  "Writes data into conn's output."
  [conn data]
  (doto (:out conn)
    (.println (json-str data))
    (.flush)))

(defn move-paddle!
  "Attempts to move paddle. If direction is same as last direction 
   does nothing. Resets last-direction and last-timestamp upon move."
  [conn direction]
  (write! conn {:msgType "changeDir" :data direction})
  direction)

(defn size-at-least?
  "Returns true if collection is at least given size."
  [n coll]
  (= n (count (take n coll))))

(defn take-ball-events
  "Returns a vector of two ball events that should be used for  
   calculating ball target."
  [[event1 event2 & _ :as events]]
  (if (size-at-least? 3 events)
    (let [event3 (nth events 2)
          angle1 (Math/abs (calc/calculate-angle event1 event2))
          angle2 (Math/abs (calc/calculate-angle event2 event3))
          diff   (Math/abs (- angle1 angle2))]
      (if (< diff 0.01)
        [event1 event2]
        (take-ball-events (rest events))))
    [event1 event2]))

(defn calculate-move
  "Calculates move speed based on data, ball events and strategy."
  [data strategy ball-events]
  (let [[event1 event2]  (take-ball-events ball-events) 
        position         (-> data :left :y)
        direction        (calc/ball-direction event1 event2)
        [angle _ target] (calc/calculate-ball-target (:conf data) direction event1 event2)        
        impact-time      nil ;(calc/time-to-target direction (:conf data) event2 event1)
        strategy-fn      (strategies/all strategy)
        movement         (strategy-fn (:conf data) position angle direction target impact-time)]
    movement))

(defn react? [timestamp]
  "Checks if given time is within the hardcoded
   time threshold."
  (let [current (System/currentTimeMillis)]
    (> (- current timestamp) 100)))

(defn make-move!
  "Moves paddle based on given data. Takes into account
   throttled response rate and doesn't make a move if
   there is not enough data available."
  [conn data strategy ball-events timestamp old-direction]
  (let [[ev1 ev2 :as events] (cons ((juxt #(get-in % [:ball :pos :x])
                                          #(get-in % [:ball :pos :y])
                                          :time) data) ball-events)]
    (merge {:events events}
           (when (and (react? timestamp) ev1 ev2)
             (let [new-direction (calculate-move data strategy events)]
               (when-not (= new-direction old-direction)
                 {:direction (move-paddle! conn new-direction)
                  :timestamp (System/currentTimeMillis)}))))))

(defn defaults
  []
  {:timestamp (System/currentTimeMillis)
   :direction nil
   :events ()
   :winners []
   :game-data []})

(defn game-over!
  "Resets game state, updates winner and prints results."
  [data winners game-data]
  (println (str "Game ended. Winner: " data))
  (spit (str "log/" (System/currentTimeMillis) ".clj") game-data)
  (let [new-winners (conj winners data)]
    (println (frequencies new-winners))
    (assoc (defaults) :winners new-winners)))

(defn handle-message!
  "Dispatches based on message."
  [conn strategy {msg-type :msgType data :data}
   ball-events game-data winners last-timestamp last-direction]
  (case msg-type
    :joined (println (str "Game joined successfully. Use following URL for visualization: " data))
    :gameStarted (println (str "Game started: " (first data) " vs. " (second data)))
    :gameIsOn (make-move! conn data strategy ball-events last-timestamp last-direction)
    :gameIsOver (game-over! data winners game-data)
    :error (println "error: " data)
    :pass))

(defn parse-message
  "Parses JSON structure into a Clojure map."
  [data]
  (try
    (update-in (read-json data true) [:msgType] keyword)
    (catch Throwable e {:msg-type :error :data (. e getMessage)})))

(defn stop
  "Stops game."
  [conn]
  (assoc conn :exit true)) 
                          
(defn read-msg
  "Reads message from input stream."
  [conn]
  (.readLine (:in conn)))

(def not-nil? (complement nil?))

(defn game-data-seq
  "Creates a sequence of messages from the input stream."
  [conn]
  (take-while not-nil? (repeatedly #(read-msg conn))))

(defn conn-handler
  "Initiates game state. Iterates through
   input and reacts if necessary."
  [conn strategy]
  (loop [messages (game-data-seq conn)
         {:keys [winners events direction timestamp game-data] :as state} (defaults)]
    (let [message (parse-message (first messages))]
      (recur
       (rest messages)
       (-> (merge state (handle-message! conn strategy message
                                         events game-data winners timestamp direction))
           (update-in [:game-data] conj (:data message))))))
  (stop conn))


(defn connect
  "Wraps servers input and output into
   a map."
  [server]
  (let [socket (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))]
    {:in in :out out}))

(defn -main
  [team-name hostname port]
  (let [conn (connect {:name hostname :port (read-string port)})
        join-message {:msgType "join" :data team-name}]
    (.mkdir (File. "log"))
    (write! conn join-message)
    (.start (Thread. #(conn-handler conn :combo)))))

(defn start
  "Initiates a game on test server."
  []
  (-main "mysema" "boris.helloworldopen.fi" "9090"))

(defn start-duel
  "Starts duel between two players"
  ([team1 team2 strategy]
  (let [conn (connect {:name "boris.helloworldopen.fi" :port 9090})
        join-message {:msgType "requestDuel" :data [team1 team2]}]
    (write! conn join-message)
    (.start (Thread. #(conn-handler conn (or strategy :combo)))))))

(comment
(use '[incanter core stats charts])

(defn view-game
  [game]
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
            (add-points right-x right-y))))))

;testing on virtual machine
