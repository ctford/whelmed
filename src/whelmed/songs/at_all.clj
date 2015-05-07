(ns whelmed.songs.at-all
  (:use
    leipzig.scale
    leipzig.melody
    leipzig.live
    leipzig.chord
    leipzig.temperament
    whelmed.melody
    whelmed.contrib.harpsichord
    whelmed.instrument)
  (:require [leipzig.temperament :as temperament]))

(defn with-bass [chord]
  (-> chord (assoc :bass (lower (:i chord)))))

(def I (-> triad (root 0) with-bass))
(def II (-> triad (root 1) with-bass))
(def V (-> triad (root 4) with-bass))

(def progression [I I II II II V I (update-in V [:bass] lower)])

(def rhythm-n-bass
  (let [bassline (phrase (cycle [4 3 1]) (mapcat #(repeat %2 (:bass %1)) progression (cycle [1 2])))
        rhythm (fn [chord]
                 (->> [nil (dissoc chord :bass)]
                      (phrase (repeat 2))))]
    (->> progression
         (mapthen rhythm)
         (with bassline)
         (all :part ::default))))

(def intro
  (->> (phrase (repeat 32 1) (cycle [5 4]) (cycle [1 2/3]))
    (all :part ::default)
    (with rhythm-n-bass)))

(def melody
  (let [rhyth (fn [n] (concat (repeat n 1/2) [(- 9 (/ n 2))]))
        stresses (concat [3/4 3/4 5/4] (repeat 1))]
    (->>
      (after -1 (phrase (rhyth 7) [2 4 5 4 4 2 4 nil] stresses))
      (then
        (after -1 (phrase (rhyth 7) [-2 1 2 1 1 -2 1 nil] stresses)))
      (then
        (after -1 (phrase (rhyth 10) [-2 1 2 1 1 -2 1 2 3 4 nil] stresses)))
      (then
        (after -1 (phrase (rhyth 10) [-1 -2 -3 0 0 -3 0 1 0 -3 nil] stresses)))
      (all :part ::dux)
      (with rhythm-n-bass))))

(def answer
  (->>
    (after 9/2 (phrase (repeat 1/2) [11 11 12 9 7]))
    (then
      (after 11/2 (phrase (repeat 1/2) [8 8 9 7 5])))
    (then
      (after 11/2 (phrase (repeat 1/2) [8 8 9 6 4])))
    (then
      (after 11/2 (phrase (repeat 1/2) [11 11 12 11 8])))
    (all :part ::comes)))

(def chorus
  (let [doesnt (phrase (concat (repeat 4 1/2) [6]) [-3 0 0 -3 0] (cons 5/4 (repeat 1)))
        doesnt-at-all (->> doesnt
                           (times 3)
                           (then
                             (->> doesnt drop-last
                                  (then (phrase [1/2 1/2 1/2 9/2] [0 1 0 -1]))))
                           (all :part ::dux)
                           (with rhythm-n-bass))
        bada (->> (phrase [1/2 1/2 3/2 2/2 9/2] [0 4 5 4 0])
                  (times 3)
                  drop-last
                  (then (phrase [5/2 1/2 1/2 10/2] [0 1 0 -1]))
                  (after 9/2)
                  (where :pitch raise)
                  (all :part ::comes))]
    (->>
      doesnt-at-all 
      (then (with doesnt-at-all bada)))))

(def finale
  (->> (phrase [1/2 1/2 1/2] [11 13 14])
    (then (after -1/2 (phrase [13/2] [(update-in I [:i] raise)])))
    (all :part ::default)))

(def at-all
  (->>
    intro
    (then melody)
    (then (->> melody (with answer)))
    (then chorus)
    (then (->> melody (with answer)))
    (then (after 3/2 (->> melody (with answer)
                          (then chorus)
                          (in-time (comp (scale [2/3 1/3]) #(* 2 %))))))
    (then finale)
    (wherever (comp not :part), :part (is ::default))
    (in-time (bpm 160))
    (where :pitch (comp temperament/equal low D major))))

(defmethod play-note ::dux [{hz :pitch s :duration stress :velocity}]
  (some-> hz (* 2) (sing s 1 :pan -1/5 :wet 0.3 :room 0.3 :volume (* 2 (or stress 2/3)) :limit 4000))
  (some-> hz (corgan s 1 :pan 1/9 :wet 0.7 :room 0.3 :vol 0.2 :limit 1000 :vibrato 8/3))) 

(defmethod play-note ::default [{s :duration hz :pitch stress :velocity}]
  (some-> hz (corgan s :under-attack 0 :limit 1000 :depth 1/2 :pan 0 :vibrato 8/3 :wet 0.1 :room 0.3 :vol 0.08))) 

(defmethod play-note ::comes [{hz :pitch s :duration stress :velocity}]
  (some-> hz (harpsichord s :pan 1/5 :vibrato 4/3 :room 0.3 :depth 0.5 :wet 0.5 :limit 1500 :vol 1.5))) 

(comment
  (play at-all)
  (jam (var at-all))
) 
