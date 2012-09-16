(ns pingpong.test.core
  (:use pingpong.core midje.sweet))

(def example-data
  {:time 1336219278079,
   :left {:y 186.0 :playerName "JohnMcEnroe"}
   :right {:y 310.0 :playerName "BorisBecker"}
   :ball {:pos {:x 291.0 :y 82.0}}
   :conf {:maxWidth 640 :maxHeight 480 :paddleHeight 50 :paddleWidth 10 :ballRadius 5 :tickInterval 15}})

(facts "calculate-x-at-y"
  (calculate-x-at-y 0 [0 1] 1) => 0)

(facts "out-of-bounds"
  (out-of-bounds 100 120) => :over
  (out-of-bounds 100 -10) => :under)

(facts "ball-direction"
  (ball-direction [1 0] [2 0]) => :left
  (ball-direction [2 0] [1 0]) => :right)

(facts "ball target calculation"
  (let [calc (ball-target-calculator 640 480 50 10 5)]
    (calc [20 10] [30 20])   => [15 5]    ; top corner    
    (calc [20 460] [30 450]) => [15 465]  ; bottom corner
    (calc [80 20] [90 40])   => [15 120]  ; top reflection
    (calc [80 470] [90 460]) => [15 415]  ; bottom reflection
    (calc [40 45] [50 50])   => [15 65/2] ; down    
    (calc [40 45] [50 40])   => [15 115/2]; up
    (calc [40 40] [50 40])   => [15 40])) ; straight    

(facts "paddle target calculation"
  (let [calc (paddle-destination-calculator example-data)
        max-height (-> example-data :conf :maxHeight)
        paddle-height (-> example-data :conf :paddleHeight)
        center-position (- (/ max-height 2) (/ paddle-height 2))]
    (calc [20 10] [30 20])   => 0     ; top corner    
    (calc [20 460] [30 450]) => 430   ; bottom corner
    (calc [80 20] [90 40])   => 95    ; top reflection
    (calc [80 470] [90 460]) => 390   ; bottom reflection
    (calc [40 45] [50 50])   => 15/2  ; down    
    (calc [40 45] [50 40])   => 65/2  ; up
    (calc [40 40] [50 40])   => 15))  ; straight
       