(ns whelmed.songs.in-the-mood
  (:use
    [leipzig.melody]
    [leipzig.canon]
    [whelmed.melody]
    [whelmed.instrument]
    [leipzig.scale])
  (:require
    [overtone.live :as overtone]
    [overtone.inst.drum :as drums]))

(defn mapthen [f notes] (->> notes (map f) (reduce #(then %2 %1))))
(defn swing [beat] ((scale [2/3 1/3]) (* 2 beat)))

(defn tap [drum times length]
  (map #(zipmap [:time :duration :drum] [%1 (- length %1) drum]) times))

(def riff
  (let [progression [0 0 3 0 4 0]
        melody (->>
          (take 11 (phrase (repeat 1/2) (cycle [-5 -3 0]))) 
          (then (phrase [5/2] [0]))
          (where :part (is ::melody))) 
        bassline (fn [root] (->>
          (phrase (repeat 1) [0 2 4 5 7 5 4 2])
          (where :pitch (from root))
          (where :pitch (comp low low low))
          (where :part (is ::bassline)))) 
        beat (->>
          (tap :tick [1 3 5 7] 8)
          (with (tap :kick [0 1/2 3/2 5/2 7/2 8/2 10/2 11/2 13/2] 8))
          (where :part (is ::beat)))
        chords (fn [root]
          (let [bass (phrase [7 1] (repeat root))]
            (->> bass
              (with ((interval 2) bass)) 
              (with ((interval 4) bass)) 
              (where :part (is ::chords)))))]
  (->>
    (->> melody (times 4) (then (->> melody (where :pitch inc))) (then melody))
    (with (mapthen bassline progression))
    (with (mapthen chords progression))
    (where :pitch (comp G major))
    (with (->> beat (times 6)))
    (in-time swing)
    (in-time (bpm 180)))))

(def kit {:kick drums/kick2
          :tick drums/closed-hat
          :tock drums/open-hat})

(defmethod play-note ::bassline [note] (pick 0.40 0.3 note))
(defmethod play-note ::melody [note] (pick 0.99 0.3 note))
(defmethod play-note ::chords [note] (pick 0.99 0.1 note))
(defmethod play-note ::beat [note] ((-> note :drum kit)))

(comment
  (def riff nil) 
  (jam riff)
) 
