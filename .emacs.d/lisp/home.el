;; EXWM Plugins
(use-package perspective-exwm
	:demand t)

(use-package buffer-move
	:demand t)

;; Custom Functions
(defun zonai/desktop-manual () (interactive)
			 (get-buffer-create "Zonai/Desktop-Manual")
			 (switch-to-buffer "Zonai/Desktop-Manual") 

			 (find-file "~/.emacs.d/.custom/Desktop-Manual.org")
			 (org-present)

			 (message "Here is the Manual!")
			 (read-only-mode)) 

(defun zonai/autostart (cmd)
	"Start CMD unless already running."
	(let ((buf-name (concat "*" cmd "*")))
		(unless (process-live-p (get-buffer-process buf-name))
			(start-process-shell-command cmd buf-name cmd))))

(defun zonai/set-command (command &optional buffer)
	"Start shell COMMAND in the background. If BUFFER is provided, log process output to that buffer."
	(interactive (list (read-shell-command "Run: ")))
	(start-process-shell-command command buffer command))

(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun zonai/switch-start (buffer cmd)
	"Switch to buffer with name BUFFER or start one with CMD."
	(if-let (b (get-buffer buffer))
			(switch-to-buffer b)
		(zonai/set-command cmd)))

(defun zonai/update-wallpaper ()
	(interactive)
	(start-process-shell-command
	 "feh" nil "feh --bg-scale ~/.emacs.d/.custom/wallpapers/wind-waker.jpg"))

(defun zonai/exwm-input-set-key (key command)
	"Similar to `exwm-input-set-key', but always refreshes prefix keys.
  This allows defining keys from any place in config."
	(exwm-input-set-key key command)
	;; Alternatively, try general-setq (which calls customize handler)
	(exwm-input--update-global-prefix-keys))

(defun zonai/exwm-next-workspace ()
	(interactive)
	(other-frame 1))

(defun zonai/move-tab-other-frame ()
	(interactive)
	(tab-bar-move-tab-to-frame nil))

(defun zonai/set-bar ()
	(interactive)
	(zonai/set-command "./System/Applications/eww/target/release/eww daemon")
	(start-process-shell-command
	 "eww" nil "./System/Applications/eww/target/release/eww open bar"))

(defun zonai/exwm-init-hook ()
	(zonai/set-command "sudo sysctl kernel.unprivileged_userns_clone=1") ;; Let User use AppImages (Hardened-Kernel)
	(zonai/set-command "xset r rate 200 60" nil)
	(zonai/set-command "xset b off" nil) ;; Disable Annoying X Beep Noise
	(run-at-time "2 sec" nil (lambda () (zonai/update-wallpaper)))
	(zonai/set-bar)
	(zonai/set-command "picom --daemon" nil)
	(zonai/set-command "dunst" nil))

;; EXWM Configuration
(use-package exwm
	:demand t
	:init
  (setq mouse-autoselect-window nil
        focus-follows-mouse t)
	;; Stops asking to replace current Window Manager
	(setq-default exwm-replace nil)
	(perspective-exwm-mode)
	:config
	(setq exwm-workspace-number 8)
	;; the next two make all buffers available on all workspaces
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

	;; Start the init-hook & hide modeline on all X Windows
	(add-hook 'exwm-init-hook #'zonai/exwm-init-hook)
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
	
	(setq perspective-exwm-override-initial-name
				'((0 . "emacs")
					(1 . "web")
					(2 . "music")
					(3 . "discrd")
					(4 . "audio")
					(5 . "term")
					(6 . "game")
					(7 . "ctrl")
					(8 . "&othr")))

	;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

	;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
				'(?\s-\ ;; Super+Space
					?\C-\
					?\M-x
					?\M-&
					?\M-h
					?\M-j
					?\M-k
					?\M-l
					?\M-H
					?\M-J
					?\M-K
					?\M-L
					?\M-:))  

	;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

	;; Launch Application Functions
	(defun exwm/run-browser ()
		(interactive)
		(start-process-shell-command
		 "browser" nil "firefox")
		(exwm-workspace-switch-create 1)
		(exwm-layout-toggle-mode-line))

	(defun exwm/run-discord ()
		(interactive)
		(start-process-shell-command
		 "discord" nil "discord")
		(exwm-workspace-switch-create 3))

	(defun exwm/run-spotify ()
		(interactive)
		(start-process-shell-command
		 "spotify" nil "./.local/bin/spotify.sh")
		(exwm-workspace-switch-create 2))

	(defun exwm/run-pavucontrol ()
		(interactive)
		(start-process-shell-command
		 "ctrl" nil "pavucontrol")
		(exwm-workspace-switch-create 4))
	
	(defun exwm/run-slippi ()
		(interactive)
		(start-process-shell-command
		 "slippi-launcher" nil "./System/Applications/Slippi/Slippi-Launcher.AppImage")
		(exwm-workspace-switch-create 6))

	;; Move Window to Workspace Functions
	(defvar workspace-number)
	(defun exwm/move-win-to-0 ()
		(interactive)
		(setq workspace-number 0)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-1 ()
		(interactive)
		(setq workspace-number 1)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-2 ()
		(interactive)
		(setq workspace-number 2)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-3 ()
		(interactive)
		(setq workspace-number 3)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-4 ()
		(interactive)
		(setq workspace-number 4)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-5 ()
		(interactive)
		(setq workspace-number 5)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-6 ()
		(interactive)
		(setq workspace-number 6)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-7 ()
		(interactive)
		(setq workspace-number 7)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(defun exwm/move-win-to-8 ()
		(interactive)
		(setq workspace-number 8)
		(let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
					(id (exwm--buffer->id (window-buffer))))
			(exwm-workspace-move-window frame id)))

	(setq exwm-input-global-keys
				`(;; EXWM Management
					([?\s-r] . exwm-reset)
					([?\s-Q] . kill-emacs)					
					;; EXWM
					([?\s-e] . perspective-exwm-switch-perspective)
					([?\s-b] . exwm-layout-toggle-mode-line)
					;; Window Management
					([?\s-w] . kill-current-buffer)
					([?\s-W] . kill-buffer-and-window)
					([?\s-i] . split-window-vertically)
					([?\s-o] . split-window-horizontally)
					([?\s-f] . exwm-layout-toggle-fullscreen)
					;; Move Window to Workspace
					([?\s-~] . exwm/move-win-to-0)
					([?\s-!] . exwm/move-win-to-1)
					([?\s-@] . exwm/move-win-to-2)
					([?\s-#] . exwm/move-win-to-3)
					([?\s-$] . exwm/move-win-to-4)
					([?\s-%] . exwm/move-win-to-5)
					([?\s-^] . exwm/move-win-to-6)
					([?\s-&] . exwm/move-win-to-7)
					([?\s-*] . exwm/move-win-to-8)
					;; Window Movement
					([?\M-h] . windmove-left)
					([?\M-j] . windmove-down)
					([?\M-k] . windmove-up)
					([?\M-l] . windmove-right)
					([?\M-H] . buf-move-left)
					([?\M-J] . buf-move-down)
					([?\M-K] . buf-move-up)
					([?\M-L] . buf-move-right)
					;; Workspaces
					([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
					,@(mapcar (lambda (i)
											`(,(kbd (format "s-%d" i)) .
												(lambda ()
													(interactive)
													(exwm-workspace-switch-create ,i))))
										(number-sequence 0 9))))

	;; Use input-set-key for KeyChords :)
   ;; Applications
	  (exwm-input-set-key (kbd "s-SPC b") 'exwm/run-browser)
		(exwm-input-set-key (kbd "s-SPC D") 'exwm/run-discord)
		(exwm-input-set-key (kbd "s-SPC S") 'exwm/run-spotify)
		(exwm-input-set-key (kbd "s-SPC p") 'exwm/run-pavucontrol)
		(exwm-input-set-key (kbd "s-SPC m") 'exwm/run-slippi)

	 ;; EXWM Management
		(exwm-input-set-key (kbd "s-SPC s") 'switch-to-buffer)
		(exwm-input-set-key (kbd "s-SPC w") 'delete-window)
;;		(exwm-input-set-key (kbd "s-SPC ") ')

		(exwm-enable))

(use-package desktop-environment
	:demand t
	:after exwm
	:config (desktop-environment-mode)
	:custom
	(desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
	(desktop-environment-screenshot-command "flameshot gui"))
