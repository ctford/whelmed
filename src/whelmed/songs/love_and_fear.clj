(ns whelmed.songs.love-and-fear
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

(defn bass [chord element] (assoc chord :bass (-> chord element low)))
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
     [:i :v :i]))

(def chords
  (->> progression
    (map #(cluster %1 (vals %2))
      [2 2 4])
    (reduce #(then %2 %1))))

(def arpeggios 
  (let [one
    (->> progression
      (map #(arpeggiate %2 %1 1/2)
           [[:i :iii :v :vii] 
            [:v- :i :iii :v] 
            [:i :v :vii :ix :vii]])
      (reduce #(then %2 %1))
      (but 12/2 13/2 (partial where :time inc))
      (but 14/2 15/2 (partial where :duration (from 1/2))))
        two (->> one
             (but 2 8 (is (after 2
               (phrase [1/2 1/2 1/2 1/2 4] [5 4 2 -1 0])))))]
    (->> one (then two))))

(comment
  (->> chords
    (times 2)
    (with arpeggios)
    (times 2)
    (where :time (bpm 90))
    (where :duration (bpm 90))
    (where :pitch (comp G minor))
    play)

)

