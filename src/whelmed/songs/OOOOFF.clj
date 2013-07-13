(ns whelmed.songs.OOOOFF
  (:use
    [leipzig.melody]
    [leipzig.live]
    [whelmed.melody]
    [leipzig.scale] 
    [overtone.inst.sampled-piano]
    [whelmed.instrument]) 
  (:require
    [overtone.live :as overtone]
    [overtone.inst.drum :as drums]
    [overtone.synth.stringed :as strings])) 

(def riff
  (->>
    (phrase (repeat 1/2) (range 6 -6 -1))
    (with (phrase (repeat 1/2) (range -6 6))) 
    (with (phrase [1/4 1/4 2 1/4 1/4 2] [7 6 7 6 5 6]))
    (with (phrase [1/4 1/4 1/4 1/4 2] [-6 -5 -6 -7 -6]))
    (in-time (bpm 120))
    (where :part (is ::melody))
    (where :pitch (comp E flat blues))))

(defmethod play-note ::melody [note] (pick 0.99 0.3 note))

(defmethod play-note :melody
  [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (overtone/at (+ start duration) (overtone/ctl id :gate 0))))

(comment
  (def riff nil) 
  (jam (var riff))
) 
