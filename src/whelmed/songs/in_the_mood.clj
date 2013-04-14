(ns whelmed.songs.in-the-mood
  (:use
    [leipzig.melody]
    [leipzig.canon]
    [leipzig.chord]
    [leipzig.temperament]
    [whelmed.melody]
    [whelmed.instrument]
    [leipzig.scale])
  (:require
    [overtone.live :as overtone]
    [overtone.inst.drum :as drums]))

(defn swing [beat] ((scale [3/5 2/5]) (* 2 beat)))

(def riff
  (let [progression [0 0 3 0 4 0]
        melody (->>
          (take 11 (phrase (repeat 1/2) (cycle [-5 -3 0]))) 
          (then (phrase [5/2] [0]))
          (where :part (is ::melody))) 
        bassline (fn [base] (->>
          (phrase [1 1 1 1/2 1/2 1 1 1 1]  [0 2 4 5 4 7 5 4 2])
          (where :pitch (from base))
          (where :pitch (comp low low))
          (where :part (is ::bassline)))) 
        beat (->>
          (after 1 (rhythm [2 2 2 1])) 
          (where :part (is ::beat)))
        chords (fn [base] (->>
          (strum triad [7 1])
          (where :pitch (from base))
          (where :part (is ::chords))))]
  (->>
    (->> melody (times 4) (then (->> melody (where :pitch inc))) (then melody))
    (with (mapthen bassline progression))
    (with (mapthen chords progression))
    (where :pitch (comp C major))
    (filter #(-> % :part (= ::bassline)))
    (with (->> beat (times 6)))
    (in-time swing)
    (in-time (bpm 180)))))

;(defmethod play-note ::bassline [note] (pick 0.40 0.3 note))
(defmethod play-note ::bassline [{pitch :pitch duration :duration}]
  (brassy ((pythagorean (C 0)) pitch) duration 0.8 0.9))
;(defmethod play-note ::melody [note] (pick 0.88 0.3 note))
;(defmethod play-note ::chords [note] (pick 0.99 0.1 note))
(defmethod play-note ::beat [note] (drums/open-hat))

(comment
  (def riff nil) 
  (jam riff)
) 
