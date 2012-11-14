(ns whelmed.songs.dolorem-ipsum
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [leipzig.canon]
    [whelmed.instrument]
    [overtone.live :only [stop midi->hz]]))

; Extra concepts
(defn arpeggiate [chord ks duration]
  (map 
    (fn [k time] {:time time :pitch (chord k) :duration duration})
    ks
    (reductions + 0 (repeat duration))))

(defn raise [chord k n] (update-in chord [k] #(+ % n)))

(defn inversion [chord n]
  (cond
    (= n 1)
      (-> chord (root -7) (raise :i 7)) 
    (= n 2)
      (-> chord (inversion 1) (raise :iii 7)))) 

(def sixth (-> triad (assoc :vi 5)))

; Melody
(def neque
  (->>
    (phrase
      [1/2 1/2 1/2 1/4 1/4 1/2 1/2 1/2 1/4 1/4]
      [4 4 5 4 5 6 8 5 4 5])
    (times 2)
    (but 3.5 4 (is
                 (->> (phrase [1/8 1/8 1/8 1/8] [4 5 4 5])
                   (after 3.5))))
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
      [5 4 2 3 4 7 6 6 5 4 3 4 2.5])
    (where :part (is ::melody))))

(def it (->> (reduce with
                     [(phrase [1] [7]) (phrase [2] [4]) (phrase [3] [0])])
          (where :part (is ::melody))))

; Arpeggios
(def theme 
  (let [up
          #(-> % (raise :iii 1) (raise :v 1))
        chords
          [triad (up triad) (up (up triad)) (up triad)]]
    (->> chords
      (map #(arpeggiate % [:v :i :iii :v] 1/4))
      (reduce #(then %2 %1))
      (times 2)
      (where :part (is ::arpeggios)))))

(def response
  (->>
      (arpeggiate (-> triad (root 4) (inversion 2))
        [:i :v :i :iii] 1/4)
      (times 4)
    (then
      (->> (arpeggiate (-> sixth (root 1))
             [:v :iii :i :vi] 1/4)
        (times 4)))
    (where :part (is ::arpeggios))))

(def wander
  (->> 
      (arpeggiate (-> triad (root 2))
        [:iii :i :iii :v] 1/4)
      (times 4)
      (but 15/4 16/4 #(where :pitch inc %))
    (then
      (->> (arpeggiate (-> sixth (root 2))
             [:v :iii :i :vi] 1/4)
        (times 4)))
    (then
      response)
    (then
      (->> (arpeggiate (-> triad (root 4) (inversion 2))
             [:i :v :i :iii] 1/4)
        (times 4)))
    (then
      (->> (arpeggiate
             (-> triad (root 4) (inversion 2) (raise :i -3/2))
             [:i :v :i :iii] 1/4)
        (times 4)))
    (where :part (is ::arpeggios))))

(def air
  (->>
    (map #(times 4 (phrase [1/4] [%])) [0 -4 0 -5 0 0 0])
    (reduce #(then %2 %1))
    (where :part (is ::arpeggios))))

(def ends (->> (phrase [1/4] [7]) (where :part (is ::arpeggios))))

; Oooh
(def aaah
  (->> (phrase [1 1] [6 5]) (times 4)
    (where :part (is ::oooh))))

(def oooh-aaah
  (->>
    (phrase
      [1 1 1 1]
      [2 3 4 3])
    (canon (interval -5))
    (with (phrase [1 1 1 1] [0 0 0 0]))
    (times 2)
    (then
      (->> aaah (with (phrase [2 2 2 2] [4 4 3 3]))
        (with (phrase [1 1 1 1] [1 1 1 1]))))
    (where :part (is ::oooh))))

(def la-la-la-la
  (->>
    (phrase [2 1 1/2 1/2 2 1 1/2 1/2 4]
            [4 8 6 4 2 8 6 4 1])
    (then (phrase [4] [3]))
    (times 2)
    (where :part (is ::oooh))))

(def wa-wa-wa-wa
  (->>
    (phrase [4 4 4 4 4 4] [4 7 8 10 11 8])
    (where :part (is ::oooh))))

; Pull it all together
(def dolorem-ipsum
  (let [lorem
          (->> theme (then response))
        intro
          (->> lorem (with (->> neque (then sit-amet))) (times 2))
        development
          (->> wander (with notice))
        finale
          (with it ends)]
    (->> lorem
      (then intro) (then development)
      (then (->> theme (but 4 8 (partial where :pitch high))))
      (then (->> theme (with neque)))
      (then oooh-aaah)
      (then (->> intro (with la-la-la-la)
              (then (with development wa-wa-wa-wa))
              (then air) (then finale)))
      (where :time (bpm 80))
      (where :duration (bpm 80))
      (where :pitch (comp F lydian)))))

; The arrangement
(defmethod play-note ::melody [{:keys [pitch duration]}]
  (bell (midi->hz pitch) (* 7 duration)))
(defmethod play-note ::arpeggios [{:keys [pitch]}]
  (sawnoff (midi->hz (- pitch 24))))
(defmethod play-note ::oooh [{:keys [pitch duration]}]
  (groan (midi->hz pitch) (* 2 duration) 1/4))

;(->> dolorem-ipsum play)
