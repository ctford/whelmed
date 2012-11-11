(ns whelmed.songs.dolorem-ipsum
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

(def neque
  (->>
    (phrase
      [1/2 1/2 1/2 1/4 1/4 1/2 1/2 1/2 1/4 1/4]
      [4 4 5 4 5 6 8 5 4 5])
    (where :part (is ::melody))))

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
        (times 4)
        (but 15/4 16/4 #(where :pitch inc %)))
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

(def end (phrase [1/4] [7]))

(def dolorem-ipsum
  (let [intro
         (->> (times 2 theme) (then response) (where :part (is ::arpeggios))
           (with (->> (times 2 neque) (where :part (is ::melody))))
           (times 2))
        development (->> wander (where :part (is ::arpeggios)))
        finale (->> end (where :part (is ::arpeggios)))]
    (->> intro (then development) (then intro) (then finale) 
      (where :time (bpm 80))
      (where :duration (bpm 80))
      (where :pitch (comp F lydian)))))

(defmethod play-note ::arpeggios [{:keys [pitch]}]
  (-> pitch (- 12) midi->hz sawish))

(defmethod play-note ::melody [{:keys [pitch duration]}]
  (-> pitch midi->hz bell))

;(->> dolorem-ipsum play)
