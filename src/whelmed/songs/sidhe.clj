(ns whelmed.songs.sidhe
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.canon]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.contrib.organ-cornet]
    [whelmed.instrument])
  (:require [overtone.live :as overtone]
            [overtone.inst.drum :as drums]
            [overtone.synth.stringed :as strings]))
(def beat 
  (->>
    [(->> (rhythm [3/2 1/2 1/2 3/2]) (where :drum (is :kick)))
     (->> (rhythm [2 1]) (after 1) (where :drum (is :tock)))
     (->> (rhythm [1/2]) (after 7/2) (where :drum (is :tick)))]
    (reduce with)
    (times 8)
    (where :part (is ::beat))))

(def bassline
  (->>
    (phrase (repeat 4) (range 0 -8 -1))
    (where :pitch lower)
    (where :part (is ::default))))

(def flourishes 
  (let [first-flourish (phrase
                         [1/4 1/4 3/2 1 1 9/2]
                         [1 2 3 4 2 1])
        second-flourish (phrase (map :duration first-flourish)
                                [1 2 3 2 1 0])]
    (->>
      (phrase [5/2 1/2 1/2 8/2] [4 3 2 4])
      (then first-flourish)
      (then (phrase [5/2 1/4 1/4 9/2] [4 2 3 4]))
      (then second-flourish) 
      (where :part (is ::default)))))

(def harmony
  (->> bassline
       (where :pitch (from 9))
       (wherever #(-> % :time (= 12)) :pitch (from 1/2))
       (where :part (is ::default))))

(def melody
  (->>
    (phrase
      [1/2 1 1/2 2 1/2 1 1/2 2 1/2 1/2 1/2 1 1 1/2 4]
      [-4 0 0 0 2 4 5 4 4 4 3 2 2 3 4])
    (then
      (phrase
        [1/2 1/2 1 2 1/2 1/2 1 2]
        [4 4 3 2 3 4 6 4]))
    (then
      (phrase
        [1/2 3/2 1/2 3/2 1/2 7/2]
        [2 1 2 1 -1 0]))
    (after -1/2)
    (where :part (is ::default))))

(def lead-in
  (->>
    (phrase
      (repeat 1/2)
      [-3 0 2 1 0])
    (after -3)
    (where :part (is ::default))))

(def chords
  (->>
    [(-> ninth (root 0))
     (-> seventh (root 4))
     (-> seventh (root 3))
     (-> triad (root 4) (update-in [:iii] (from 1/2)))
     (-> seventh (root 3))
     (-> triad (root 2))
     (-> seventh (root 4))
     (-> seventh (root 0))]
    (phrase (repeat 4))
    (where :pitch raise)
    (where :part (is ::chords))))

(def kit {:kick drums/kick2 
          :tick drums/closed-hat,
          :tock drums/open-hat})

(defmethod play-note ::beat [note] ((-> note :drum kit)))
(defmethod play-note ::default [note] (pick 0.99 0.1 note))
(defmethod play-note ::chords [{midi :pitch, length :duration}]
  (organ-cornet (overtone/midi->hz midi) length 0.1))

(def sidhe
  (->>
    (with bassline (drop 4 harmony))
    (then (reduce with [bassline harmony flourishes]))
    (then (reduce with [beat bassline harmony flourishes chords]))
    (then (reduce with [bassline harmony lead-in melody]))
    (then (reduce with [bassline harmony melody beat]))
    (wherever :pitch, :pitch (comp C minor))
    (where :time (bpm 100))
    (where :duration (bpm 100))))
