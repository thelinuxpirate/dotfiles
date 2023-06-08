(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Improve Start Time
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)
(setq desktop-restore-forces-onscreen nil)

;; UI
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(global-display-line-numbers-mode . 1) default-frame-alist)
(set-fringe-mode 10)

;; Font, check /usr/share/fonts
;(push '(font . "Source Code Pro") default-frame-alist)
;(set-face-font 'default "Source Code Pro")
;(set-face-font 'variable-pitch "DejaVu Sans")
;(copy-face 'default 'fixed-pitch)

;; Prevent n00b instructions
(setq server-client-instructions nil)

(provide 'early-init)
