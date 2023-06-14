(setq package-enable-at-startup nil)
(setq make-backup-files nil)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)
(setq desktop-restore-forces-onscreen nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(set-fringe-mode 10)

(setq server-client-instructions nil)
(provide 'early-init)
