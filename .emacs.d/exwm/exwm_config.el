(defun exwm-update-class ()
   (exwm-workspace-rename-buffer exwm-class-name))

;;(defun exwm-update-title ()
;;     (pcase exwm-class-name
;;       ("Brave-browser" (exwm-workspace-rename-buffer (format "Brave: %s" exwm-title)))
;;       ("Spotify"       (exwm-workspace-rename-buffer (format "Spotify: %s" exwm-title)))
;;       ("discord"       (exwm-workspace-rename-buffer (format "Biscord: %s" exwm-title))))
;; )
;; (defun exwm-configure-window-by-class ()
;;   (interactive)
;;     (pcase exwm-class-name
;;       ("Brave-browser" (exwm-workspace-move-window 0))
;;       ("discord"       (exwm-workspace-move-window 3))
;;       ("Spotify"       (exwm-workspace-move-window 5) 
;;                          (exwm-floating-toggle-floating)
;; 		           (exwm-layout-toggle-mode-line))))

(use-package exwm
  :config
  (setq mouse-autoselect-window nil
        focus-follows-mouse t
        exwm-workspace-warp-cursor t 
	exwm-workspace-number 7)
  ;;When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm-update-class)
  ;; Update panel indicator when workspace changes
  (add-hook 'exwm-workspace-switch-hook #'polybar--send-polybar-exwm-workspace)
  ;; Update "title"
  ;;(add-hook 'exwm-update-title-hook #'exwm-update-title)
  ;; Configure Window By Class:
  ;;(add-hook 'exwm-update-finish-hook #'exwm-configure-window-by-class)
  ;; Hide the modeline on all floating windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
  ;; Disable these two:
  (display-battery-mode -1)
  (display-time-mode -1)

;; Set the screen resolution (update this to be the correct resolution for your screen!)
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
	    (start-process-shell-command
	     "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 0x0 --rotate normal")))
(exwm-randr-enable)

;; MAYBE? This section killed Exwm for me;

(defvar polybar--polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun polybar--kill-piratebar ()
  (interactive)
  (when polybar--polybar-process
    (ignore-errors
      (kill-process polybar--polybar-process)))
  (setq polybar--polybar-process nil))

(defun polybar--start-piratebar ()
  (interactive)
  (polybar--kill-piratebar)
  (setq polybar--polybar-process (start-process-shell-command "polybar" nil "polybar piratebar")))

(defun polybar--send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun polybar--send-polybar-exwm-workspace ()
  (polybar--send-polybar-hook "exwm-workspace" 1))

;; REMEMBER TO ADD HOOK
(defvar xmobar--xmobar-process nil
  "Holds the process of the running XMobar instance, if any")

(defun xmobar--kill-bar ()
  (interactive)
  (when xmobar--xmobar-process
    (ignore-errors
      (kill-process xmobar--xmobar-process)))
  (setq xmobar--xmobar-process nil))

(defun xmobar--start-bar ()
  (interactive)
  (xmobar--kill-bar)
  (setq xmobar--xmobar-process (start-process-shell-command "xmobar" nil "xmobar /home/thelinuxpirate/.emacs.d/xmobar/xmobarrc_1")))

;;(defun xmobar--send-xmobar-hook (module-name hook-index)
;;  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

;;(defun polybar--send-polybar-exwm-workspace ()
;;  (polybar--send-polybar-hook "exwm-workspace" 1))

;; These keys should always pass through to Emacs;
(setq exwm-input-prefix-keys
  '(?\C-x
    ?\s-j
    ?\s-w
    ?\M-x))

;; Ctrl+Q will enable the next key to be sent directly
;;(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Super + j:
(global-set-key (kbd "s-j s-e") 'execute-extended-command)
(global-set-key (kbd "s-j k") 'kill-current-buffer)
(global-set-key (kbd "s-j s-k") 'kill-buffer)
(global-set-key (kbd "s-j l") 'switch-to-buffer)
(global-set-key (kbd "s-j b") 'list-buffers)
(global-set-key (kbd "s-j j") 'switch-to-prev-buffer)
(global-set-key (kbd "s-j s-j") 'switch-to-next-buffer)
;; Super + w:
(global-set-key (kbd "s-w w") 'make-frame)
(global-set-key (kbd "s-w 1") 'delete-window)
(global-set-key (kbd "s-w 2") 'delete-other-windows)
(global-set-key (kbd "s-w 3") 'split-window-below)
(global-set-key (kbd "s-w 4") 'split-window-right)
(global-set-key (kbd "s-w 5") 'split-window-horizontally)
(global-set-key (kbd "s-w 6") 'split-window-vertically)

;; Custom Actions:
(defun start--brave-browser ()
  (interactive)
  (start-process-shell-command "brave" nil "brave-browser"))
(defun start--discord ()
  (interactive)
  (start-process-shell-command "discord" nil "discord"))
(defun start--spotify ()
  (interactive)
  (start-process-shell-command "spotify" nil "spotify"))
(defun start--gimp ()
  (interactive)
  (start-process-shell-command "gimp" nil "gimp"))
  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Move between windows
          ([s-l] . windmove-left)
          ([s-h] . windmove-right)
          ([s-k] . windmove-up)
          ([s-j] . windmove-down)
          ([?\s-`] . (lambda () 
	               (interactive) (exwm-workspace-switch-create 0)))
         ,@(mapcar (lambda (i)
                     `(,(kbd (format "s-%d" i)) .
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
                   (number-sequence 0 6))))
          (exwm-input-set-key (kbd "<s-return>") 'vterm)
	  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
	  (exwm-input-set-key (kbd "s-Q") 'kill-emacs)
	  (exwm-input-set-key (kbd "s-d") 'dired)
	  (exwm-input-set-key (kbd "s-D") 'start--discord)
	  (exwm-input-set-key (kbd "s-B") 'start--brave-browser)
	  (exwm-input-set-key (kbd "s-m") 'exwm-layout-hide-mode-line)
	  (exwm-input-set-key (kbd "s-M") 'exwm-layout-show-mode-line)
	  (exwm-input-set-key (kbd "s-S") 'start--spotify)
	  (exwm-input-set-key (kbd "s-r") 'exwm-floating-toggle-floating)
	  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
	  (exwm-input-set-key (kbd "s-P") 'polybar--kill-piratebar)
	  (exwm-input-set-key (kbd "s-p") 'polybar--start-piratebar) 
	  (exwm-input-set-key (kbd "s-c") 'exwm-input-release-keyboard)
	  (exwm-input-set-key (kbd "s-x") 'exwm-reset)

;; Move Windows:
	(defun exwm-move-window-to-workspace(workspace-number)
	    (interactive)
	        (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
		    (id (exwm--buffer->id (window-buffer))))
		        (exwm-workspace-move-window frame id)))
	;; Switch to Workspace 0: 
	(exwm-input-set-key (kbd "s-~")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 0)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 0)))))
	(exwm-input-set-key (kbd "s-)")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 0)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 0)))))
       ;; Move Window to Workspace 1 
       (exwm-input-set-key (kbd "s-!")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 1)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 1)))))

       (exwm-input-set-key (kbd "s-@")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 2)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 2)))))

       (exwm-input-set-key (kbd "s-#")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 3)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 3)))))

       (exwm-input-set-key (kbd "s-$")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 4)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 4)))))

	(exwm-input-set-key (kbd "s-%")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 5)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 5)))))

	(exwm-input-set-key (kbd "s-^")
                (lambda()
                  (interactive)
                  (exwm-move-window-to-workspace 6)
                  (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 6)))))

(exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  
  (desktop-environment-brightness-small-increment "1%+")
  (desktop-environment-brightness-small-decrement "1%-")
  (desktop-environment-brightness-normal-increment "2%+")
  (desktop-environment-brightness-normal-decrement "2%")
  (desktop-environment-volume-small-increment "1%+")
  (desktop-environment-volume-small-decrement "1%-")
  (desktop-environment-volume-normal-increment "2%+")
  (desktop-environment-volume-normal-decrement "2%-"))
