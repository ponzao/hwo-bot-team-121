(ns pingpong.strategies-test
  (:use midje.sweet))

(def conf 
  {:maxWidth     640 
   :maxHeight    480 
   :paddleHeight 50 
   :paddleWidth  10
   :ballRadius   5 
   :tickInterval 15})

(facts "ball moves right"
  (let [angle (calc/calculate-angle [620 10] [610 15])
        