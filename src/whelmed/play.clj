(ns whelmed.play
  (:use
    [overtone.live :only [recording-start recording-stop]]
    [leipzig.melody :only [play then after]]
    [whelmed.songs.west :only [west-with-the-sun]]
    [whelmed.songs.dolorem-ipsum :only [dolorem-ipsum]]
    [whelmed.songs.love-and-fear :only [love-and-fear]]
    [whelmed.songs.at-all :only [at-all]]
    [whelmed.songs.SKA :only [ska]]))

(def tracks
  [["ska" ska]
   ["west" west-with-the-sun]
   ["dolorem" dolorem-ipsum]
   ["love" love-and-fear]
   ["at-all" at-all]])

(def track-map (reduce #(assoc %1 (first %2) (second %2)) {} tracks))

(defn run [music]
  (play music)
  (->>
    music
    last
    ((fn [{:keys [time duration]}] (+ time duration)))
    Thread/sleep))

(defn -main

  ([track-name file-name]
   (recording-start file-name)
   (-main track-name)
   (recording-stop)
   (System/exit 0))

  ([track-name]
   (->>
     track-name
     track-map
     run)
   (System/exit 0))

  ([]
    (->>
      tracks
      (map second)
      (reduce #(then (after 2000 %2) %1))
       run)
   (System/exit 0)))
