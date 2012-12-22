(ns whelmed.songs.love-and-fear
  (:use
    [leipzig.melody]
    [leipzig.canon]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz at ctl]]))

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
          (phrase [1 1/2 1/2 1 1/2 9/2] [0 0 2 -1 -3 -2])
        three        
          (phrase (concat (repeat 8 1/2) [4]) [0 -1 -3 -7 -5 -3 -1 2 5])] 
    (->> one (then two) (then one) (then three)
      (where :pitch low)
      (where :part (is ::bass)))))

(def chords
  (->> progression
    (map #(cluster %1 (vals %2))
      [2 2 4])
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
            [:i :v :vii :ix :vii]])
      (reduce #(then %2 %1))
      (but 11/2 12/2 (partial where :duration inc))
      (but 12/2 13/2 (partial where :time inc))
      (but 14/2 15/2 (partial where :duration (from 1/2))))
        two (->> one
             (but 2 8 (is (after 2
               (phrase [1/2 1/2 1/2 1/2 4] [5 4 2 -1 0])))))]
    (->> one (then two) (times 2)  
      (but 27 32 (is (->> (phrase [1 4] [7 6]) (after 27))))
      (where :part (is ::arpeggios)))))

(def theme
  (->> (phrase [2 2 9/2] [6 6 7])
    (canon (interval -2))
    (where :part (is ::melody))))

(def melody
  (let [aaaaand [1 2] 
        rhythm [1/2 3/2 1/2 1 1 1 1] 
        there-are-only-two-feelings 
          (->>
            (phrase
              (concat aaaaand rhythm)
              [4 6 6 6 6 7 6 5 6])
            (after -2) (then theme))
        love-and-fear
          (->> (phrase rhythm [9 9 8 7 6 4 6]) (after 1) (then theme))
        there-are 
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
  (->> there-are-only-two-feelings (then love-and-fear)
    (then (times 2 (->> there-are (then only-two-activities))))
    (where :part (is ::melody)))))


; Arrangement
(defmethod play-note ::melody [{midi :pitch}] (-> midi midi->hz (bell 5000)))

(def love-and-fear
  (let [intro
          (->> bassline 
            (with arpeggios))
        statement
          (->> melody
            (with (times 2 chords))         
            (with (after 32 (with arpeggios bassline))))
        oh-love-and-fear 
          (->> (phrase [1/2 1/2 1 1/2 1/2 1 1/2 1/2 4]
                       [2 1 0 0 -1 0 2 3 2])
            (after -1)
            (canon (interval 7))) 
        outro (->> chords
               (with (->> (after -1/2 theme) (with oh-love-and-fear)
                       (times 2)))
                (times 2)
                (then (take 6 oh-love-and-fear)))]

  (->> intro (then (times 2 statement)) (then outro) 
    (where :duration (bpm 80))
    (where :time (bpm 80))
    (where :pitch (comp G minor)))))

(comment
 (play love-and-fear)
)

;(jam (bpm 90) (comp G minor) bassline)
