(ns whelmed.songs.love-and-fear
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

(defn bass [chord element] (assoc chord :bass (-> chord element low)))

(def progression
  (map #(-> %1 (bass %2) vals ((partial cluster %3)))
     [(-> triad (root 0)) (-> triad (root 2) (inversion 2)) (-> triad (root -2))]
     [:i :v :i]
     [4 4 8]))

(->> progression
  (reduce #(then %2 %1))
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (where :pitch (comp G minor))
  play)

