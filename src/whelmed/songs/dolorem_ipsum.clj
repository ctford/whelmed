(ns whelmed.songs.west
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

(defn cluster [pitches duration]
  (map
    #(zipmap
      [:time :duration :pitch]
      [0 duration %])
    pitches))

(defn progress [duration chords]
  (->> chords 
    (map vals)
    (map #(cluster % duration))
    (reduce #(then %2 %1))))

(defn arpeggiate [chord [k & ks] duration]
  (if k (then
          (arpeggiate chord ks duration)
          [{:time 0 :pitch (chord k) :duration duration}])
    []))

(defn y [chord element f] (update-in chord [element] f))
(def raise #(-> % (y :iii inc) (y :v inc)))

(def theme (let [base (triad 0)]
             (->>
               [base (raise base) (raise (raise base)) (raise base)]
               (map #(arpeggiate % [:v :i :iii :v] 1/4))
               (reduce #(then %2 %1)))))

(def response
  (->>
      (->> (arpeggiate (raise (triad 1)) [:iii :i :iii :v] 1/4)
        (times 4))
    (then
      (->> (arpeggiate (triad 1) [:iii :i :iii :v] 1/4)
        (times 4)))))

(def dolorem-ipsum
  (->> theme (times 2) (then response) (times 2)
    (where :time (bpm 100))
    (where :duration (bpm 100))
    (where :pitch (comp F lydian))))

(defn demo
  ([notes] (demo notes major))
  ([scale notes]
    (->> notes
      (where :time (bpm 90))
      (where :duration (bpm 90))
      (where :pitch (comp C scale))
      play)))

;(->> dolorem-ipsum play)

;(defmethod play-note ::bass [{:keys [pitch]}] (-> pitch midi->hz groan))
