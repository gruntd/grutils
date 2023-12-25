(defun mpv/run (&rest option-plists)
  "Runs instances of the mpv command with configurable options, concurrently."
  (dolist (options option-plists)
    (sb-thread:make-thread
     (lambda ()
       (let* ((window-title (getf options :window-title ""))
              (dimensions (getf options :dimensions "640x480"))
              (force-window (getf options :force-window t))
              (shuffle (getf options :shuffle nil))
              (fullscreen (getf options :fullscreen nil))
              (mute-vids (getf options :mute-vids nil))
              (loop-vids (getf options :loop-vids nil))
              (vid-dir (getf options :vid-dir ""))
              (vid-dir-path (if (string-right-trim "*" vid-dir)
                                (concatenate 'string vid-dir "*")
                                vid-dir))
              (shuffle-flag (if shuffle "--shuffle" ""))
              (fullscreen-flag (if fullscreen "--fullscreen" ""))
              (mute-flag (if mute-vids "--mute=yes" ""))
              (loop-flag (if loop-vids "--loop-playlist" ""))
              (command (format nil "mpv ~@[--title=~A~] ~@[--geometry=~A~] ~@[--force-window=~A~] ~A ~A ~A ~A ~A"
                               window-title dimensions (if force-window "yes" "no") shuffle-flag fullscreen-flag mute-flag loop-flag vid-dir-path)))
         (uiop:run-program command :output t :wait nil))))))


(mpv/run '(:window-title "GRUTILS-STREAM-STARTUP"
                        :dimensions "1920x1080"
                        :shuffle t
                        :fullscreen t
                        :loop-vids t
                        :vid-dir "/home/jost/grutils/stream/player/vids/memes/")
         '(:window-title "GRUTILS-BACKGROUND VIDEO"
                        :dimensions "1920x1080" 
                        :shuffle t 
                        :fullscreen t
                        :mute-vids t
                        :loop-vids t
                        :vid-dir "/home/jost/grutils/stream/player/vids/bgs/"))



