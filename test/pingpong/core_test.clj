(ns pingpong.core-test
  (:use pingpong.core
        midje.sweet))

(facts "size at least"
  (size-at-least? 2 [1 2 3]) => true
  (size-at-least? 4 [1 2 3]) => false)

(facts "not nil"
  (not-nil? 15)  => true
  (not-nil? nil) => false)

(facts "take ball events"
       (let [ev0 [0 1]
             ev1 [1 1]
             ev2 [2 2]
             ev3 [3 3]]
         (take-ball-events [ev1 ev2]) => [ev1 ev2]
         (take-ball-events [ev1 ev2 ev3]) => [ev1 ev2]
         (take-ball-events [ev0 ev1 ev2 ev3]) => [ev1 ev2]))

; duel results

;{combo 9, basic 2}
;{combo 10} vs accelerating
;{combo 8, zigzag 2}
;{combo 5, corner 5}
;{combo 7, anti-corner 3}

;{corner 5, anti 6}

;{combo 7, becker 3}
;{combo 17, becker 3}

;{anti-corner 7, becker 3}
;{anti-corner 13, becker 7}