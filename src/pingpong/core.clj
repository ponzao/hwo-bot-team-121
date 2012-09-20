(ns pingpong.core
  (:use [clojure.data.json :only (read-json json-str)]
        [clojure.tools.logging :only (info error)]
        [clojure.string :only (join)])
  (:require [pingpong.calc :as calc]
            [pingpong.strategies :as strategies])
  (:import [java.net Socket]
           [java.io PrintWriter InputStreamReader BufferedReader])
  (:gen-class :main true))

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

(defn size-at-least?
  [n coll]
  (= n (count (take n coll))))

(defn take-ball-events [[event1 event2 & _ :as events]]
  (if (size-at-least? 3 events)
    (let [event3 (nth events 2)
          angle1 (Math/abs (calc/calculate-angle event1 event2))
          angle2 (Math/abs (calc/calculate-angle event2 event3))
          diff   (Math/abs (- angle1 angle2))]
      (if (< diff 0.01)
        [event1 event2]
        (take-ball-events (rest events))))
    [event1 event2]))

(defn calculate-move [data ball-events]
  (let [[event1 event2]  (take-ball-events ball-events) 
        position         (-> data :left :y)
        [angle _ target] (calc/calculate-ball-target (:conf data) event1 event2)
        direction        (calc/ball-direction event1 event2)
        toimpact         (calc/time-left-to-hit-target direction (:conf data) event2 event1)
        movement         (strategies/corner (:conf data) position angle direction target toimpact)]
    movement))

(defn time-diff [last-timestamp]
  (let [current (System/currentTimeMillis)
        old last-timestamp]
    (> (- current old) 100)))

(defn make-move! [conn {{{ball-x :x ball-y :y} :pos} :ball
                        {paddle-y :y} :left :as data} ball-events last-timestamp last-direction]
  (swap! ball-events conj [ball-x ball-y (:time data)])
  (when (time-diff @last-timestamp) ; only react max 10 times / sec
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
