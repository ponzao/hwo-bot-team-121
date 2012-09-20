(ns pingpong.calc-test
  (:use midje.sweet
        pingpong.calc))

(def conf 
  {:maxWidth     640 
   :maxHeight    480 
   :paddleHeight 50 
   :paddleWidth  10
   :ballRadius   5 
   :tickInterval 15})

(def data {:conf conf})

(facts "calculate-x-at-y"
  (calculate-x-at-y 0 [0 1] 1) => 0)

(facts "out-of-bounds"
  (out-of-bounds 100 120) => :over
  (out-of-bounds 100 -10) => :under)

(facts "ball-direction"
  (ball-direction [1 0] [2 0]) => :left
  (ball-direction [2 0] [1 0]) => :right)

(facts "ball target calculation"
  (let [calc (partial calculate-ball-target conf)]
    (calc [20 10] [30 20])   => [1    15 5]    ; top corner    
    (calc [20 460] [30 450]) => [-1   15 465]  ; bottom corner
    (calc [80 20] [90 40])   => [-2   15 120]  ; top reflection
    (calc [80 470] [90 460]) => [1    15 415]  ; bottom reflection
    (calc [40 45] [50 50])   => [1/2  15 65/2] ; down    
    (calc [40 45] [50 40])   => [-1/2 15 115/2]; up
    (calc [40 40] [50 40])   => [0    15 40])) ; straight    

(comment (facts "paddle target calculation"
  (let [calc (comp #(nth % 2) (partial calculate-paddle-target data))]
    (calc [20 10] [30 20])   => -20   ; top corner    
    (calc [20 460] [30 450]) => 440   ; bottom corner
    (calc [80 20] [90 40])   => 95    ; top reflection
    (calc [80 470] [90 460]) => 390   ; bottom reflection
    (calc [40 45] [50 50])   => 15/2  ; down    
    (calc [40 45] [50 40])   => 65/2  ; up
    (calc [40 40] [50 40])   => 15)))  ; straight
