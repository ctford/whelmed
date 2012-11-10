(ns whelmed.songs.dolorem-ipsum
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

(def theme 
  (->>
    [triad (raise triad) (raise (raise triad)) (raise triad)]
    (map #(arpeggiate % [:v :i :iii :v] 1/4))
    (reduce #(then %2 %1))))

(def response
  (->>
      (->> (arpeggiate
             (raise (-> triad (root 1)))
             [:iii :i :iii :v] 1/4)
        (times 4))
    (then
      (->> (arpeggiate
             (-> (-> triad (root 1)) (assoc :vi 6))
             [:v :iii :i :vi] 1/4)
        (times 4)))))

(def wander
  (->> 
      (->> (arpeggiate
             (-> (-> triad (root 2)))
             [:iii :i :iii :v] 1/4)
        (times 4))
    (then
      (->> (arpeggiate
             (-> (-> triad (root 2)) (assoc :vi 7))
             [:v :iii :i :vi] 1/4)
        (times 4)))
    (then
      response)
    (then
      (->> (arpeggiate
             (raise (-> triad (root 1)))
             [:iii :i :iii :v] 1/4)
        (times 4)))
    (then
      (->> (arpeggiate
             (-> (raise (-> triad (root 1))) (update-in [:iii] #(- % 3/2)))
             [:iii :i :iii :v] 1/4)
        (times 4)))))

(def dolorem-ipsum
  (->> theme (times 2) (then response) (times 2) (then wander)
    (where :time (bpm 80))
    (where :duration (bpm 80))
    (where :pitch (comp F lydian))))

(defn demo
  ([notes] (demo notes major))
  ([scale notes]
    (->> notes
      (where :time (bpm 80))
      (where :duration (bpm 80))
      (where :pitch (comp C scale))
      play)))

;(->> wander demo)

;(->> dolorem-ipsum play)

;(defmethod play-note ::bass [{:keys [pitch]}] (-> pitch midi->hz groan))
