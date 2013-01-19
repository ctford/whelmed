(ns whelmed.instrument
  (:use
    [leipzig.melody]
    [overtone.inst.sampled-piano]
    [whelmed.contrib.organ-cornet]
    [overtone.live])
  (:require [overtone.synth.stringed :as strings])) 

(definst shudder [freq 440 vibrato 6]
  (let [envelope (env-gen (perc 2 1.5) :action FREE)]
    (*
      (* envelope (sin-osc vibrato))
      (square freq)
      (sin-osc freq))))

(definst sawish [freq 440]
  (let [envelope (env-gen (perc 0.2 1.5) :action FREE)]
    (*
      0.7
      envelope
      (+
        (square (* freq 0.99))
        (square freq)))))

(definst sinish [freq 440]
  (let [envelope (env-gen (perc 0.1 1.1) :action FREE)]
    (*
      envelope
      (sin-osc freq))))

(definst groan [freq 440 duration 10000 vibrato 8/3]
  (let [length (/ duration 1000)
        envelope (* (sin-osc vibrato)
                    (env-gen (perc 0.1 length) :action FREE))]
    (*
      0.7
      envelope
      (+
        (* (sin-osc 0.5) (+ 0.1 (saw freq)))
        (* (sin-osc 0.8) (+ -0.03 (square freq)))
        (+ -0.04 (sin-osc freq))))))

(definst bell [frequency 440 duration 1000
  h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics   [ 1  2  3  4.2  5.4 6.8]
        proportions [h0 h1 h2   h3   h4  h5]
        proportional-partial
          (fn [harmonic proportion]
            (let [envelope
                    (env-gen (perc 0.01 (* proportion (/ duration 1000))))
                  overtone
                    (* harmonic frequency)]
              (* 1/2 proportion envelope (sin-osc overtone))))
        partials
          (map proportional-partial harmonics proportions)
        whole (mix partials)]
      (detect-silence whole :action FREE)
      whole))

(definst sawnoff [freq 440 depth 10]
  (let [envelope (env-gen (perc 0.1 0.9) :action FREE)] 
    (* 
      envelope
      (sin-osc freq)
      (sin-osc (* 2 freq))
      (saw (+ freq (* depth (lf-saw:kr 0.1 0.2)))))))

(strings/gen-stringed-synth ektara 1 true)
(defn pick [distort amp  {midi :pitch, start :time, length :duration}]
  (let [synth-id (at start
         (ektara midi :distort distort :amp amp :gate 1))]
    (at (+ start length) (ctl synth-id :gate 0))))
