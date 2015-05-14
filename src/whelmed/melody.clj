(ns whelmed.melody
  (:require
    [leipzig.scale :refer [from]]))

(defn between?
  [from to]
  (fn [{:keys [time]}] 
    (and (>= time from) (< time to))))

(defn augment
  [chord k n]
  (update-in chord [k] (from n)))
