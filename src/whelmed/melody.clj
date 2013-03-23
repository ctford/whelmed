(ns whelmed.melody
  (:use
    [leipzig.melody]
    [leipzig.chord]
    [leipzig.scale]))

(defn from [base] (partial + base))

(defn but [from to f notes]
  (let [early? #(< (:time %) from)
        late? #(>= (:time %) to)
        apple (->> notes
                (filter #(or (early? %) (late? %)))) 
        core (->> notes
               (filter #(not (early? %))) 
               (filter #(not (late? %))))] 
    (with apple (f core))))

(defn demo
  ([notes] (demo major notes))
  ([scale notes]
    (->> notes
      (where :time (bpm 90))
      (where :duration (bpm 90))
      (where :pitch (comp C scale))
      play)))

(defn mapthen [f notes] (->> notes (map f) (reduce #(then %2 %1))))
(defn strum [chord durations] (mapthen #(cluster % (vals chord)) durations))
(defn cluster [duration pitches]
  (map
    #(zipmap
      [:time :duration :pitch]
      [0 duration %])
    pitches))

(defn- sum-n [series n] (reduce + (take n series)))
(defn rhythm 
  [durations]
  (let [timings (map (partial sum-n durations) (range))]
    (map #(zipmap [:time :duration] [%1 %2]) timings durations)))

(defn raise [chord k n] (update-in chord [k] (from n)))

(defn- forever [riff]
  (let [{final :time, duration :duration} (last @riff)]
    (concat
      @riff
      (lazy-seq (->> (forever riff) (where :time (from (+ final duration))))))))

(defn jam* [riff] (->> riff forever play)) 
(defmacro jam [riff] `(jam* (var ~riff)))

(defn in-time [timing notes]
  (->> notes
    (map
      (fn [{time :time, duration :duration :as note}]
        (let [relative-timing #(-> % (- time) timing (+ (timing time)))]
          (update-in note [:duration] relative-timing))))
    (where :time timing)))
