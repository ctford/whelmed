(ns whelmed.play
  (:use
    [overtone.live :only [recording-start recording-stop kill-server]]
    [leipzig.melody :only [then after duration]]
    [leipzig.live :only [play]]
    [whelmed.songs.west :only [west-with-the-sun]]
    [whelmed.songs.dolorem-ipsum :only [dolorem-ipsum]]
    [whelmed.songs.sidhe :only [sidhe]]
    [whelmed.songs.my-friend :only [my-friend]]
    [whelmed.songs.love-and-fear :only [love-and-fear]]
    [whelmed.songs.at-all :only [at-all]]
    [whelmed.songs.SKA :only [ska]]))

(def tracks
  [
   ["west" west-with-the-sun] ; A minor, 80 bpm
   ["sidhe" sidhe] ; C minor -> E minor, 105 bpm
   ["my-friend" my-friend]   ; F major, 120 bpm
   ["ska" ska] ; E minor -> B flat major -> E minor, 180 bpm -> 120 bpm
   ["dolorem" dolorem-ipsum] ; F lydian, 80 bpm
   ["love" love-and-fear] ; G minor, 80 bpm
   ["at-all" at-all] ; D major, 160
   ])

(def minutes
  (->> tracks
       (map second)
       (map duration)
       (reduce +)
       (* 1/60)
       float))

(defn lookup [track-name]
  (if (= track-name "all")
    (->>
      tracks
      drop-last
      (map second)
      (reduce #(then (after 2 %2) %1))
      (then (after (rand-int 900) at-all))) ; Secret track!
    (get
      (->>
        tracks
        (reduce concat)
        (apply hash-map))
      track-name)))

(defn play-n-wait [music]
  (-> music play deref)
  (Thread/sleep 4000))

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
   (-> track-name lookup (record file-name))
   (finish))

  ([track-name]
   (-> track-name lookup play-n-wait)
   (finish))

  ([]
   (-main "all")))
