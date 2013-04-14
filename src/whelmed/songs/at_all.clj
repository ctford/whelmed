(ns whelmed.songs.at-all
  (:use
    leipzig.scale
    leipzig.melody
    leipzig.live
    leipzig.chord
    [overtone.live :only [midi->hz]]
    whelmed.contrib.organ-cornet
    whelmed.melody
    whelmed.instrument))

(defn with-bass [chord]
  (-> chord (assoc :bass (low (:i chord)))))

(def I (-> triad (root 0) with-bass))
(def II (-> triad (root 1) with-bass))
(def V (-> triad (root 4) with-bass))

(def progression [I I II II II V I (update-in V [:bass] low)])

(def rhythm-n-bass
  (let [bass (fn [chord]
              (->> chord
                :bass
                (repeat 2)
                (phrase [3 1])))
        
        rhythm (fn [chord]
                 (->> (dissoc chord :bass)
                   vals
                   (cluster 2)
                   (after 2)))
        once #(with (rhythm %) (bass %))]
    (->> progression
      (map once)
      (reduce #(then %2 %1)))))

(def intro
  (->> (take 32 (cycle [5 4]))
    (phrase (repeat 1))
    (with rhythm-n-bass)))

(def melody
  (->>
    (after -1 (phrase (repeat 1/2) [2 4 5 4 4 2 4]))
    (then
      (after 9/2 (phrase (repeat 1/2) [-2 1 2 1 1 -2 1])))
    (then
      (after 9/2 (phrase (repeat 1/2) [-2 1 2 1 1 -2 1 2 3 4])))
    (then
      (after 6/2 (phrase (repeat 1/2) [-1 -2 -3 0 0 -3 0 1 0 -3])))
    (with rhythm-n-bass)))

(def answer
  (->>
    (after 9/2 (phrase (repeat 1/2) [11 11 12 9 7]))
    (then
      (after 11/2 (phrase (repeat 1/2) [8 8 9 8 3])))
    (then
      (after 11/2 (phrase (repeat 1/2) [8 8 9 6 4])))
    (then
      (after 11/2 (phrase (repeat 1/2) [11 11 12 11 8])))))

(def finale
  (->> (phrase [1/2 1/2 1/2] [11 13 14])
    (then (after -1/2 (->> (update-in I [:i] high)
            vals
            (cluster 13/2))))))

(def at-all
  (->>
    intro
    (then melody)
    (then (->> melody (with answer)))
    (then (after 3/2 (->> melody (with answer)
            (where :time #( * % 2/3))
            (where :duration #( * % 2/3)))))
    (then finale)
    (where :part (fnil identity ::default))
    (where :time (bpm 160))
    (where :duration (is 200))
    (where :pitch (comp low G major))))

(defmethod play-note ::default [{midi :pitch}]
  (organ-cornet (midi->hz midi) 150 8.0)) 

(comment
  (play at-all)
) 
