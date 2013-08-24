(ns whelmed.play
  (:use
    [overtone.live :only [recording-start recording-stop kill-server]]
    [leipzig.melody :only [then after]]
    [leipzig.live :only [play]]
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

(def lookup-track
  (reduce #(assoc %1 (first %2) (second %2)) {} tracks))

(defn play-n-wait [music]
  (->> music play deref))

(defn record [music file-name]
   (recording-start file-name)
   (play-n-wait music) 
   (recording-stop))

(defn finish []
  (Thread/sleep 2000)
  (kill-server)
  (System/exit 0))

(defn -main

  ([track-name file-name]
   (-> track-name lookup-track (record file-name))
   (finish))

  ([track-name]
   (-> track-name lookup-track play-n-wait)
   (finish))

  ([]
    (->>
      tracks
      (map second)
      (reduce #(then (after 2000 %2) %1))
       play-n-wait)
   (finish)))
