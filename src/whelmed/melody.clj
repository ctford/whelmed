(ns whelmed.melody
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.chord]
    [leipzig.scale]))

(defn between? [from to]
  (fn [note] 
    (and (>= (:time note) from) (< (:time note) to))))

(defn but [from to replacement notes]
  (->> notes
    (filter (comp not (between? from to)))
    (with (after from replacement))))

(defn mapthen  [f & args]
  (->> args
       (apply map f)
       (reduce #(then %2 %1))))

(defn augment [chord k n] (update-in chord [k] (from n)))

(defn in-time [timing notes]
  (->> notes
    (map
      (fn [{time :time, duration :duration :as note}]
        (let [relative-timing #(-> % (- time) timing (+ (timing time)))]
          (update-in note [:duration] relative-timing))))
    (where :time timing)))

(defn demo
  ([notes] (demo major notes))
  ([scale notes]
    (->> notes
      (in-time (bpm 90))
      (where :pitch (comp C scale))
      play)))
