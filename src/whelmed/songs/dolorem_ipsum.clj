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
(def theme (let [base (triad 0)
                 raise #(-> % (y :iii inc) (y :v inc))]
             (->>
               [base (raise base) (raise (raise base)) (raise base)]
               (map #(arpeggiate % [:v :i :iii :v] 1/2))
               (reduce #(then %2 %1)))))

(def response
  (->>
    [(triad 4) (triad 4) (triad 1) (triad 1)]
    (progress 4)))

(def dolorem-ipsum
  (->> theme (times 2) (then response) (times 2)
    (where :time (bpm 120))
    (where :duration (bpm 120))
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
