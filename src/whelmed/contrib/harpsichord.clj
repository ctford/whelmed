(ns whelmed.contrib.harpsichord
  (:require
    [overtone.live :refer :all]
    [overtone.helpers.lib :refer :all]))

; Originally written by Phil Potter and copied from the Overtone examples

(defn string
  [freq duration]
  (with-overloaded-ugens
    (+ (* (line:kr 1 1 duration)
          (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
            1 1 (/ 1 freq) (* duration 2) 0.25))))) 

(definst harpsichord [freq 440 vol 1.0 pan 0.0 wet 0.5 room 0.5 limit 5000]
  (let [duration 1
        snd  (string freq duration)
        t1   (* 0.2 (string (* 2/1 freq) duration))
        t2   (* 0.15 (string (* 3/2 freq) duration))
        t3   (* 0.1 (string (* 4/3 freq) duration))
        t4   (* 0.1 (string (* 5/4 freq) duration))
        full-snd  (+ snd (mix [t1 t2 t3 t4]))]
    (-> full-snd (* vol) (free-verb :mix wet :room room) (lpf limit) (pan2 pan))))
