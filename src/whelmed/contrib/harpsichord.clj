(ns whelmed.contrib.harpsichord
  (:use [overtone.live]
        [overtone.helpers.lib]))

; Originally written by Phil Potter and copied from the Overtone examples

(defn string
 [freq duration]
 (with-overloaded-ugens
   (* (line:kr 1 1 duration FREE)
      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
             1 1 (/ 1 freq) (* duration 2) 0.25)))) 

(definst harpsichord [freq 440]
  (let [duration 1
        snd  (string freq duration)
        t1   (* 0.2 (string (* 2/1 freq) duration))
        t2   (* 0.15 (string (* 3/2 freq) duration))
        t3   (* 0.1 (string (* 4/3 freq) duration))
        t4   (* 0.1 (string (* 5/4 freq) duration))
        snd  (+ snd (mix [t1 t2 t3 t4]))]
    snd))
