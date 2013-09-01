(ns whelmed.songs.at-all
  (:use
    leipzig.scale
    leipzig.melody
    leipzig.live
    leipzig.chord
    [overtone.live :only [midi->hz]]
    whelmed.melody
    whelmed.instrument))

(defn with-bass [chord]
  (-> chord (assoc :bass (lower (:i chord)))))

(def I (-> triad (root 0) with-bass))
(def II (-> triad (root 1) with-bass))
(def V (-> triad (root 4) with-bass))

(def progression [I I II II II V I (update-in V [:bass] lower)])

(def rhythm-n-bass
  (let [bass (fn [chord]
               (phrase [3 1] (repeat (:bass chord))))
        rhythm (fn [chord]
                 (->> [(dissoc chord :bass)]
                   (phrase [2])
                   (after 2)))
        once #(with (rhythm %) (bass %))]
    (mapthen once progression)))

(def intro
  (->> (phrase (repeat 1) (cycle [5 4]))
    (take 32)
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

(def chorus
  (let [doesnt (phrase (concat (repeat 4 1/2) [6]) [-3 0 0 -3 0])
        doesnt-at-all (->> doesnt
                           (times 3)
                           (then
                             (->> doesnt drop-last
                                  (then (phrase [1/2 1/2 1/2 9/2] [0 1 0 -1]))))
                           (with rhythm-n-bass)
                           )
        bada (->> (phrase [1/2 1/2 3/2 2/2 9/2] [0 4 5 4 0])
                  (times 3)
                  drop-last
                  (then (phrase [5/2 1/2 1/2 10/2] [0 1 0 -1]))
                  (after 9/2)
                  (where :pitch raise)
                  (where :part (is ::long)))]
    (->>
      doesnt-at-all 
      (then (with doesnt-at-all bada)))))

(def finale
  (->> (phrase [1/2 1/2 1/2] [11 13 14])
    (then (after -1/2 (phrase [13/2] [(update-in I [:i] raise)])))))

(def at-all
  (->>
    intro
    (then melody)
    (then (->> melody (with answer)))
    (then chorus)
    (then (->> melody (with answer)))
    (then (after 3/2 (->> melody (with answer)
                          (then chorus)
                          (in-time (comp (scale [2/3 1/3]) #(* 2 %)))
                          (in-time #(* % 7/8)))))
    (then finale)
    (wherever (comp not :part), :part (is ::default))
    (in-time (bpm 160))
    (where :pitch (comp low D major))))

(defmethod play-note ::long [{midi :pitch ms :duration}]
  (organ (midi->hz midi) ms 2)) 
(defmethod play-note ::default [{midi :pitch}]
  (organ (midi->hz midi) 150 3)) 

(comment
  (play at-all)
) 
