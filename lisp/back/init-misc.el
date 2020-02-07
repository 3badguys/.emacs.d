;; init-misc.pl --- Config for misc

;; emms
(eval-after-load 'emms
  '(progn
     (emms-all)
     (setq emms-player-list '(emms-player-mplayer-playlist
                              emms-player-mplayer
                              emms-player-mpg321
                              emms-player-ogg123
                              emms-player-vlc
                              emms-player-vlc-playlist))))

(global-set-key (kbd "C-c e v") 'emms-playlist-mode-go)
(global-set-key (kbd "C-c e g") 'emms-play-directory)
(global-set-key (kbd "C-c e r") 'emms-random)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e SPC") 'emms-pause)

(provide 'init-misc)
