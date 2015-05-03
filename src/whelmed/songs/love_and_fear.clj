(ns whelmed.songs.love-and-fear
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.canon]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument])
  (:require [overtone.live :as overtone]
            [overtone.inst.drum :as drums]
            [overtone.synth.stringed :as strings]
            [leipzig.temperament :as temperament]))

(defn harmonise [f notes]
  (->> notes
       (all :part ::melody)
       (canon (comp (partial all :part ::harmony) f) )))

(defn tap [drum times length]
  (map #(zipmap [:time :duration :drum] [%1 (- length %1) drum]) times))

(def beata 
  (->>
    (reduce with
      [(tap :tock [1 3 5 7] 8) 
      (tap :tick [15/4 30/4] 8) 
      (tap :kick [0 3/4 6/4 10/4 14/4 16/4 19/4 22/4 26/4] 8)])
    (all :part ::beat)))

(def beatb 
  (->>
    (reduce with
      [(tap :tick [4/4 6/4 12/4 20/4 22/4 28/4] 8) 
       (tap :kick [0 1/4 2/4 3/4 8/4 9/4 10/4 11/4 16/4 19/4 24/4 27/4] 8)])
   (all :part ::beat)))

(def kit {:kick drums/kick2 
          :tick drums/closed-hat,
          :tock drums/open-hat})

(defn base [chord element]
  (-> chord (assoc :bass (-> chord element lower))))

(defn arpeggiate [chord ks duration]
  (map
    (fn [k time] {:time time :pitch (chord k) :duration duration})
      ks
      (reductions + 0 (repeat duration))))

(def progression
  (map base
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
      (where :pitch lower)
      (all :part ::bass))))

(def vanilla-bass
  (->> (phrase [0 -1 -2] [4 4 8])
       (where :pitch (comp lower lower))
       (times 2)
       (all :part ::bass)))

(def chords
  (->> 
    (phrase [2 2 4] progression)
    (times 2)
    (with (->> (phrase [2 2 4] [6 6 7]) (where :pitch raise) (after 8)))
    (where :pitch lower)
    (all :part ::chords)
    (with vanilla-bass)))

(def arpeggios 
  (let [one
    (->> progression
      (mapthen #(arpeggiate %2 %1 1/2)
           [[:i :iii :v :vii] 
            [:v- :i :iii :v] 
            [:i :v :vii :ix :vii :iii]])
      (wherever #(-> % :time (= 11/2)), :duration (is 1))
      (wherever #(-> % :time (> 11/2)), :time (from 1/2))
      (wherever #(-> % :time ( = 7)), :duration (is 1))) 
        two (->> one
             (but 2 8 (phrase [1/2 1/2 1/2 1/2 4] [5 4 2 -1 0])))]
    (->> one (then two) (times 2)  
      (but 27 32 (phrase [1 4] [7 6]))
      (all :part ::arpeggios))))

(def theme
  (->> (phrase [2 2 9/2] [6 6 7])
    (all :part ::melody)
    (harmonise (interval -2))))

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
          (after -2)
          (harmonise (partial all :pitch 2))
          (then theme) )
        love-and-fear
        (->> (phrase rhythm [9 9 8 7 6 4 6])
             (after 1)
             (harmonise (partial all :pitch 2))
             (then theme))]
    (->> there-are-only-two-feelings (then love-and-fear))))

(def melodyb
 (let  [there-are 
          (->>
            (phrase
              [1/2 1/2 1/4 1/4 1 1/4 1/4 1/4 1/4 1 9/2]
              [2 3 triad 3 2 4 3 (-> triad (root 2) (inversion 2)) 3 2 (-> triad (root -2))])
            (after -1)) 
        only-two-activities 
          (->>
            (phrase
              [1/2 3/2 1/2 1/2 1 1/2 9/2]
              [2 3 4 3 2 1 2])
            (after -1))] 
    (->> there-are (then only-two-activities) (times 2) 
    (all :part ::melody))))

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
      (all :part ::melody)
      (with (->> chords (times 2) (all :part ::blurt))))))

(defn ring [rate]
  (->> (repeatedly #([-3 -1 2 4 6 7 9 11] (rand-int 7)))
       (phrase (repeat (int (/ 48 rate)) rate))))

(def ringing
  (with
    (all :part ::melody (ring 1))
    (all :part ::harmony (ring 1/2))))

; Arrangement
(defmethod play-note ::melody [{hz :pitch s :duration}]
  (some-> hz (bell s :volume 0.9 :position 1/9 :wet 0.4 :room 0.1))
  (some-> hz (* 2) (bell 4 :volume 0.6 :position 1/7 :wet 0.9 :room 0.9)))

(defmethod play-note ::harmony [{hz :pitch s :duration}]
  (some-> hz (bell 7 :volume 0.8 :position -1/2 :wet 0.8 :room 0.9)))

(defmethod play-note ::chords [{hz :pitch, length :duration}]
  (some-> hz (corgan length 0.8 :vol 0.2 :vibrato 2/3 :depth 0.4 :pan 1/4 :room 0.9)))

(defmethod play-note ::blurt [{:keys [pitch duration]}]
  (some-> pitch (corgan duration :depth 1 :vibrato 4/3 :vol 0.2 :pan -1/2 :room 0.9)))

(defmethod play-note ::bass [{:keys [duration pitch]}]
  (some-> pitch (corgan duration :vibrato 2/3 :limit 1000 :depth 0 :pan -1/2 :depth 0 :vol 0.3 :room 0.9)))

(defmethod play-note ::arpeggios [{:keys [pitch duration]}]
  (some-> pitch (corgan duration :vibrato 4/3 :vol 0.2 :depth 0.2 :limit 2000 :pan 1/5 :room 0.9)))

(defmethod play-note ::beat [note] ((-> note :drum kit) :amp 0.5))

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
            (harmonise (interval 7)))
        outro (->> chords
               (with (->> modified-theme (with oh-love-and-fear)
                       (times 2)))
                (times 2)
                (with (times 4 beata) ringing)
                (then (take 6 oh-love-and-fear)))]

  (->>
    intro
    (then (times 2 statement))
    (then two-motives)
    (then (->> melodyb (where :pitch lower)
            (with (times 4 beatb))
            (with (->> (times 2 chords) (all :part ::blurt)))))
    (then outro) 
    (in-time (bpm 80))
    (where :pitch (comp temperament/equal G minor)))))

(comment
  (jam (var love-and-fear))
  (play love-and-fear)
)
