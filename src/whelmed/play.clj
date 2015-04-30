(ns whelmed.play
  (:use
    [overtone.live :only [recording-start recording-stop kill-server]]
    [leipzig.melody :only [then after]]
    [leipzig.live :only [play]]
    [whelmed.songs.west :only [west-with-the-sun]]
    [whelmed.songs.dolorem-ipsum :only [dolorem-ipsum]]
    [whelmed.songs.sidhe :only [sidhe]]
    [whelmed.songs.love-and-fear :only [love-and-fear]]
    [whelmed.songs.at-all :only [at-all]]
    [whelmed.songs.SKA :only [ska]]))

(def tracks
  [["west" west-with-the-sun] ; A minor
   ["ska" ska] ; E minor
   ["sidhe" sidhe] ; C minor
   ;["dolorem" dolorem-ipsum] ; F lydian 
   ["love" love-and-fear] ; G minor
   ;["at-all" at-all] ; D major
   ])

(defn lookup [track-name]
  (if (= track-name "all")
    (->>
      tracks
      (map second)
      (reduce #(then (after 2 %2) %1)))
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
