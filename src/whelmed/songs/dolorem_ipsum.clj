(ns whelmed.songs.dolorem-ipsum
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [leipzig.canon]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

(def neque
  (->>
    (phrase
      [1/2 1/2 1/2 1/4 1/4 1/2 1/2 1/2 1/4 1/4]
      [4 4 5 4 5 6 8 5 4 5])
    (times 2)
    (but 3.5 4 (is
                 (->> (phrase [1/8 1/8 1/8 1/8] [4 5 4 5])
                   (after 3.5)
                   (where :duration (is 1/4)))))
    (where :part (is ::melody))))

(def sit-amet 
  (->>
    (phrase
      [4 1 3]
      [4 6 5])
    (where :part (is ::melody))))

(def notice
  (->>
    (phrase
      [1 5/2 1/4 1/4 2 2 4 1 2.5 1/4 1/4 4 4]
      [5 4 2 3 4 7 6 6 5 4 3 4 2.5]
      )
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
    (reduce #(then %2 %1))
    (times 2)
    (where :part (is ::arpeggios))))

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
        (times 4)))
    (where :part (is ::arpeggios))))

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
        (times 4)))
    (where :part (is ::arpeggios))))

(def aaah
  (->> (phrase [1 1] [6 5]) (times 4)
    (where :part (is ::oooh))))

(def oooh-aaah
  (->>
    (phrase
      [1 1 1 1]
      [2 3 4 3])
    (canon (interval -5))
    (times 2)
    (then
      (->> aaah (with (phrase [4 4] [4 3]))))
    (where :part (is ::oooh))))

(def ends (->> (phrase [1/4] [7]) (where :part (is ::arpeggios))))
(def it (->> (reduce with
                     [(phrase [1] [7]) (phrase [2] [4]) (phrase [3] [0])])
          (where :part (is ::melody))))

(def dolorem-ipsum
  (let [lorem (->> theme (then response))
        intro
         (->> lorem 
           (with (->> neque (then sit-amet))) (times 2))
        development (->> wander (with notice))
        finale (with it ends)]
    (->> lorem
      (then intro) (then development)
      (then (->> theme (but 4 8 (partial where :pitch high))))
      (then (->> lorem (with oooh-aaah)))
      (then (with (after 32 (times 3 aaah))
              (->> intro (then development) (then finale)))) 
      (where :time (bpm 80))
      (where :duration (bpm 80))
      (where :pitch (comp F lydian)))))

(defmethod play-note ::melody [{:keys [pitch duration]}]
  (bell (midi->hz pitch) (* 7 duration)))
(defmethod play-note ::arpeggios [{:keys [pitch]}]
  (sawnoff (midi->hz (- pitch 24))))
(defmethod play-note ::oooh [{:keys [pitch duration]}]
  (groan (midi->hz pitch) (* 2 duration)))

;(->> dolorem-ipsum play)
