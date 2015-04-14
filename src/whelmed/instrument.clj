(ns whelmed.instrument
  (:use
    [leipzig.melody]
    [overtone.live])
  (:require [overtone.synth.stringed :as strings])) 

(defsynth shudder [freq 440 vibrato 6 pan 0 wet 0.5 volume 1.0]
  (out 0
       (-> (square freq)
           (* (sin-osc freq))
           (* (env-gen (perc 2 1.5) :action FREE))
           (* (sin-osc vibrato))
           (* volume)
           (free-verb :mix wet)
           (pan2 (line:ar pan (- pan) 3.5)))))

(defsynth sawish [freq 440 duration 1500 vibrato 8/3 depth 1 volume 1.0 pan 0.0 wet 0.5]
  (let [envelope (env-gen (perc 0.01 (/ duration 1000)) :action FREE)]
    (out 0
         (-> (saw freq)
             (+ (saw (* freq 1.01)))
             (* 1/2)
             (+ (* 9 (sin-osc (* freq 1.98))))
             (clip2 0.5)
             (* 2)
             (* volume envelope)
             (rlpf (mul-add (sin-osc vibrato) (* freq depth) (* 2 freq)) 1/3)
             (free-verb :mix wet)
             (pan2 pan)))))

(defsynth groan [freq 440 duration 10000 vibrato 8/3 volume 1.0 position 0 wet 0.5 low 0.25]
  (let [length (/ duration 1000)
        envelope (* (sin-osc vibrato)
                    (env-gen (perc 0.1 length) :action FREE))]
    (out 0
         (-> (+
              (* low (sin-osc (* freq 1/2)))
              (* (sin-osc 0.5) (+ 0.1 (saw freq)))
              (* (sin-osc 0.8) (+ -0.03 (square freq)))
              (+ -0.04 (sin-osc freq)))
             (* 0.7 volume envelope)
             (free-verb :mix wet)
             (pan2 position)))))

(defsynth bell [frequency 440 duration 1000 volume 1.0 position 0 wet 0.5
                h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics   [ 1  2  3  4.2  5.4 6.8]
        proportions [h0 h1 h2   h3   h4  h5]
        proportional-partial
        (fn [harmonic proportion]
          (let [envelope (* 1/5 (env-gen (perc 0.01 (* proportion (/ duration 1000)))))
                overtone (* harmonic frequency)]
            (* 1/2 proportion envelope (sin-osc overtone))))
        partials
        (map proportional-partial harmonics proportions)
        whole (-> partials mix (* 10) (free-verb :mix wet) (pan2 position))]
    (detect-silence whole :action FREE)
    (out 0 whole)))

(definst sawnoff [freq 440 depth 10]
  (let [envelope (env-gen (perc 0.1 0.9) :action FREE)] 
    (* 
      envelope
      (sin-osc freq)
      (sin-osc (* 2 freq))
      (saw (+ freq (* depth (lf-saw:kr 0.1 0.2)))))))

(strings/gen-stringed-synth ektara 1 true)
(defn pick [distort amp {midi :pitch, start :time, length :duration}]
  (let [synth-id (at start
         (ektara midi :distort distort :amp amp :gate 1))]
    (at (+ start length) (ctl synth-id :gate 0))))

(definst brassy [freq 440 dur 1000 vol 1 growl 1]
  (lpf
    (* vol
    (+
      (* 1/2 (sin-osc (* 1 freq)) 
         (env-gen (adsr 0.15 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1000)) :action FREE))
      (* 1/3 (sin-osc (* 3 freq))
         (env-gen (adsr 0.3 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1300)) :action FREE))
      (* 1/4 (sin-osc (* 5 freq))
         (env-gen (adsr 0.3 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1500)) :action FREE))
      (* 1/7 (sin-osc (* 7 freq))
         (env-gen (adsr 0.4 0.3 0.2) (line:kr 1.0 0.0 (/ dur 2000)) :action FREE)))) 
     (+ (* 5 freq) (* (line:kr (* growl 6) 1 0.1) freq (sin-osc 50)))))

(definst woah [freq 440 duration 1000 volume 1.0]
  (let [fenv (* (env-gen (perc 0.1 (/ duration 1000))) freq)
        aenv (env-gen (perc 0.005 (/ duration 1000)) :action FREE)]
    (* volume (sin-osc fenv (* 0.5 Math/PI)) aenv)))

(definst click [volume 1.0]
  (let [envelope (env-gen (perc 0.05 0.2) :action FREE)]
    (* volume envelope (pulse 5000 100))))

(defsynth organ [freq 440 dur 1000 vol 1.0 pan 0.0 wet 0.5]
  (out 0
       (->
         (map #(sin-osc (* freq %)) (range 1 5))
         mix
         (free-verb :mix wet)
         (pan2 pan)
         (* vol)
         (* (env-gen (asr 0.1 1.0 0.5)
              (line:kr 1.0 0.0 (/ dur 1000))
              :action FREE))
         (lpf (mul-add (sin-osc 5) freq (* freq 5))))))

(definst kraft-bass [freq 440 dur 1000 vol 1.0]
  (let [envelope (env-gen (asr 0 1 1)
                          (line:kr 1.0 0.0  (/ dur 1000))
                          :action FREE)
        level (+ 100 (env-gen (perc 0 3)
                              :level-scale 6000))
        osc (mix [(saw freq)
                  (saw (* freq 1.005))
                  (pulse (/ freq 2) 0.5)])]
    (-> osc (lpf level) (* vol envelope))))
