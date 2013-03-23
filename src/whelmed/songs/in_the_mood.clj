(ns whelmed.songs.in-the-mood
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale] 
    [leipzig.canon] 
    [overtone.inst.sampled-piano]
    [whelmed.contrib.organ-cornet]
    [whelmed.instrument]) 
  (:require
    [overtone.live :as overtone]
    [clojure.math.numeric-tower :as math]
    [overtone.inst.drum :as drums]
    [overtone.synth.stringed :as strings])) 

(defn swing [beat] ((scale [2/3 1/3]) (* 2 beat)))

(defn in-time [timing notes]
  (->> notes
    (map (fn [{time :time duration :duration :as note}]
           (assoc note :duration (- (timing (+ time duration)) (timing time)))))
    (where :time timing)))

(defn tap [drum times length]
  (map #(zipmap [:time :duration :drum] [%1 (- length %1) drum]) times))

(def riff
  (let [progression [0 0 3 0 4 0]
        melody (->>
          (phrase (cycle [1/2]) (cycle [2 4 7]))
          (take 11)
          (then (phrase [5/2] [7]))
          (where :part (is ::melody))) 
        bassline (fn [root] (->>
          (phrase [1 1 1 1/2 1/2 1 1 1 1] [0 2 4 5 4 7 5 4 2])
          (where :pitch #(+ % root))
          (where :pitch (comp low low low))
          (where :part (is ::bassline)))) 
        beat (->>
          (tap :tick [1 3 5 7] 8)
          (with (tap :kick [0 1/2 3/2 5/2 7/2 8/2 10/2 11/2 13/2] 8))
          (where :part (is ::beat)))
        chords (fn [root] (->>
          ;(phrase [7 1] (repeat root)) 
          (phrase (cycle [1/4 1/4 1/2]) (cycle [root (+ root 2) (+ root 4)])) 
          (take 24)
          (where :pitch low) 
          ;(canon (comp (interval 2) (partial canon (interval 2)))) 
          (where :part (is ::chords))))]
  (->>
    (->> melody (times 4) (then (->> melody (where :pitch inc))) (then melody))
    (with (->> (map bassline progression) (reduce #(then %2 %1))))
    (with (->> (map chords progression) (reduce #(then %2 %1))))
    (with (->> beat (times 6)))
    (in-time swing)
    ;(filter (comp #(= % ::chords) :part))
    (in-time (bpm 180))
    (wherever :pitch, :pitch (comp G major)))))

(def kit {:kick drums/kick2
          :tick drums/closed-hat
          :tock drums/open-hat})

(defmethod play-note ::bassline [note] (pick 0.40 0.3 note))
(defmethod play-note ::melody [note] (pick 0.90 0.3 note))
(defmethod play-note ::chords [{midi :pitch, duration :duration, start :time}]
  (let [id (sampled-piano midi 0.5)]
    (overtone/at (+ start duration) (overtone/ctl id :gate 0))
                                                ))
(defmethod play-note ::beat [note] ((-> note :drum kit)))
;(defmethod play-note ::chords [{midi :pitch, length :duration}]
;  (organ-cornet (overtone/midi->hz midi) length))

(comment
  (def riff nil) 
  (jam riff)
) 
