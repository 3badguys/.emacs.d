;;; init-xah-fly-keys.el --- Config for xah-fly-keys

;; must come before loading xah-fly-keys
(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty") ; required

;; possible layout values:

;; "azerty"
;; "azerty-be"
;; "colemak"
;; "colemak-mod-dh"
;; "dvorak"
;; "programer-dvorak"
;; "qwerty"
;; "qwerty-abnt"
;; "qwertz"
;; "workman"
;; "norman"

(xah-fly-keys 1)

;; Make Escape key do emacs's Ctrl+g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(provide 'init-xah-fly-keys)
