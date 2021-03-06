(defproject pingpong "1.0.0-SNAPSHOT"
  :description "pingpong"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.2"]
                 [org.clojure/tools.logging "0.2.3"]
                 [incanter/incanter "1.3.0"]
                 [midje "1.4.0"]]
  :dev-dependencies [[midje "1.4.0"]
                     [lein-midje "1.0.10"]]
  :profiles {:dev {:plugins [[lein-midje "2.0.0-SNAPSHOT"]]}}
  :main pingpong.core)
