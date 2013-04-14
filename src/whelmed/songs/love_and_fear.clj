(ns whelmed.songs.love-and-fear
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

(defn tap [drum times length]
  (map #(zipmap [:time :duration :drum] [%1 (- length %1) drum]) times))

(def beata 
  (->>
    (reduce with
      [(tap :tock [1 3 5 7] 8) 
      (tap :tick [15/4 30/4] 8) 
      (tap :kick [0 3/4 6/4 10/4 14/4 16/4 19/4 22/4 26/4] 8)])
    (where :part (is ::beat))))

(def beatb 
  (->>
    (reduce with
      [(tap :tick [4/4 6/4 12/4 20/4 22/4 28/4] 8) 
       (tap :kick [0 1/4 2/4 3/4 8/4 9/4 10/4 11/4 16/4 19/4 24/4 27/4] 8)])
   (where :part (is ::beat))))

(def kit {:kick drums/kick2 
          :tick drums/closed-hat,
          :tock drums/open-hat})

(defn bass [chord element]
  (-> chord (assoc :bass (-> chord element low))))

(defn arpeggiate [chord ks duration]
  (map
    (fn [k time] {:time time :pitch (chord k) :duration duration})
      ks
      (reductions + 0 (repeat duration))))

(def progression
  (map bass
     [(-> seventh (root 0))
      (-> triad (assoc :v- -3) (root 2))
      (-> ninth (root -2))]
     [:i :v- :i]))

(def bassline
  (let [one
          (phrase (concat [1 1/2 1/2 1 1/2 1/2] (repeat 8 1/2))
            [0 0 -3 -1 -1 -3 -2 -5 -2 -5 -2 -5 -2 -1])
        two
          (phrase [1 1/2 1/2 1 1/2 9/2] [0 0 2 -1 -3 -2])] 
    (->> one (then two) (times 2) 
      (where :pitch low)
      (where :part (is ::bass)))))

(def chords
  (->> progression
    (map #(cluster %1 (vals %2)) [2 2 4])
    (reduce #(then %2 %1))
    (times 2)
    (with (->> (phrase [2 2 4] [6 6 7]) (where :pitch high) (after 8)))
    (where :pitch low)
    (where :part (is ::chords))))

(def arpeggios 
  (let [one
    (->> progression
      (map #(arpeggiate %2 %1 1/2)
           [[:i :iii :v :vii] 
            [:v- :i :iii :v] 
            [:i :v :vii :ix :iii :vii]])
      (reduce #(then %2 %1))
      (wherever (between? 11/2 12/2), :duration (from 1/2))
      (wherever (between? 12/2 13/2), :time inc)
      (wherever (between? 14/2 15/2), :duration (from 1/2)))
        two (->> one
             (but 2 8 (phrase [1/2 1/2 1/2 1/2 4] [5 4 2 -1 0])))]
    (->> one (then two) (times 2)  
      (but 27 32 (phrase [1 4] [7 6]))
      (where :part (is ::arpeggios)))))

(def theme
  (->> (phrase [2 2 9/2] [6 6 7])
    (canon  (interval -2))
    (where :part (is ::melody))))

(def modified-theme
  (->> theme
    (wherever (between? 0 2), :duration (is 3/2))
    (wherever (between? 2 5), :time (from -1/2))))

(def melodya
  (let [aaaaand [1 2] 
        rhythm [1/2 3/2 1/2 1 1 1 1] 
        there-are-only-two-feelings 
          (->>
            (phrase
              (concat aaaaand rhythm)
              [4 6 6 6 6 7 6 5 6])
            (after -2) (then theme))
        love-and-fear
          (->> (phrase rhythm [9 9 8 7 6 4 6]) (after 1) (then theme))]
  (->> there-are-only-two-feelings (then love-and-fear)
    (where :part (is ::melody)))))

(def melodyb
 (let  [there-are 
          (->>
            (phrase
              [1/2 1/2 1/4 1/4 1 1/4 1/4 1/4 1/4 1 9/2]
              [2 3 4 3 2 4 3 4 3 2 2])
            (after -1)) 
        only-two-activities 
          (->>
            (phrase
              [1/2 3/2 1/2 1/2 1 1/2 9/2]
              [2 3 4 3 2 1 2])
            (after -1))] 
    (->> there-are (then only-two-activities) (times 2) 
    (where :part (is ::melody)))))

(def melody (->> melodya (then melodyb)))

(def two-motives
  (let [two
         (phrase [1/2 1/2 3/2] [2 1 0]) 
        motives (->> two (then
         (phrase [1/2 1 1/2 1 5/2] [0 0 0 1 0]))) 
        procedures ( ->> two (then 
         (phrase [1/2 1 1/2 7/2] [-1 0 -1 -3])))
        results (->> two (then
         (phrase [1/2 1 1/2 1 1 1 1 3/2 1/2 1 1/2 5]
                 [0 0 0 1 0 -1 0 0 -1 0 -1 -3])))] 
    (->> motives (then procedures) (then results) (after -1)
      (where :part (is ::melody))
      (with (->> chords (times 2) (where :part (is ::blurt)))))))

;(def two-motives nil)

; Arrangement
(defmethod play-note ::melody [note] (pick 0.99 0.3 note))
(defmethod play-note ::chords [{midi :pitch, length :duration}]
  (organ-cornet (overtone/midi->hz midi) length 0.1))
(defmethod play-note ::blurt [note]
  (pick 0.3 0.1 (-> note (update-in [:duration] (is 500)))))
(defmethod play-note ::bass [note] (pick 0.99 0.1 note))
(defmethod play-note ::arpeggios [note] (pick 0.99 0.1 note))
(defmethod play-note ::beat [note]
  ((-> note :drum kit)))

(defn when-present [f] (fn [attribute] (when attribute (f attribute))))

(def love-and-fear
  (let [intro (with bassline arpeggios)
        statement
          (->> melody
            (with (times 4 beata))
            (with (times 2 chords))         
            (with (after 32
                         (->> arpeggios
                           (with bassline)
                           (with (times 4 beatb))))))
        oh-love-and-fear 
          (->> (phrase [1/2 1/2 1 1/2 1/2 1 1/2 1/2 4]
                       [2 1 0 0 -1 0 2 3 2])
            (after -1)
            (canon (interval 7))
            (where :part (is ::melody))) 
        outro (->> chords
               (with (->> modified-theme (with oh-love-and-fear)
                       (times 2)))
                (times 2)
                (with (times 4 beata))
                (then (take 6 oh-love-and-fear)))]

  (->>
    intro
    (then (times 2 statement))
    (then two-motives)
    (then (->> melodyb (where :pitch low)
            (with (times 4 beatb))
            (with (->> (times 2 chords) (where :part (is ::blurt))))))
    (then outro) 
    (where :duration (when-present (bpm 80)))
    (where :time (when-present (bpm 80)))
    (where :pitch (when-present (comp G minor))))))

(comment
 (play love-and-fear)
)
