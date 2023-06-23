;; EXWM Plugins
(use-package perspective-exwm
	:demand t)

(use-package buffer-move
	:demand t)

;; EXWM Configuration
(use-package exwm
	:demand t
	:init
  (setq mouse-autoselect-window nil
        focus-follows-mouse t)
	;; Stops asking to replace current Window Manager
	(setq-default exwm-replace nil)

	;; Custom Functions, placed in init so they can be called upon later
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

	(defun zonai/start-command (command &optional buffer)
		"Start shell COMMAND in the background. If BUFFER is provided, log process output to that buffer."
		(interactive (list (read-shell-command "Run: ")))
		(start-process-shell-command command buffer command))

	(defun zonai/switch-start (buffer cmd)
		"Switch to buffer with name BUFFER or start one with CMD."
		(if-let (b (get-buffer buffer))
				(switch-to-buffer b)
			(zonai/start-command cmd)))

	(defun zonai/exwm-input-set-key (key command)
		"Similar to `exwm-input-set-key', but always refreshes prefix keys.
    This allows defining keys from any place in config."
		(exwm-input-set-key key command)
		;; Alternatively, try general-setq (which calls customize handler)
		(exwm-input--update-global-prefix-keys))

	(defun zonai/exwm-next-workspace ()
		(interactive)
		;; (let ((cur exwm-workspace-current-index)
		;;       (max exwm-workspace-number))
		;;   (exwm-workspace-switch (% (+ cur 1) max)))
		(other-frame 1))

	(defun zonai/move-tab-other-frame ()
		(interactive)
		(tab-bar-move-tab-to-frame nil))

	(defun zonai/exwm-init-hook ()
		(perspective-exwm-mode))

	:config
	(setq exwm-workspace-number 10)
	;; the next two make all buffers available on all workspaces
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

	(zonai/exwm-init-hook)
	(setq perspective-exwm-override-initial-name
				'((0 . "emacs")
					(1 . "web")
					(2 . "music")
					(3 . "discrd")
					(4 . "audio")
					(5 . "term")
					(6 . "game")
					(7 . "ctrl")))

	;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

	;; (with-eval-after-load 'evil
  ;;   (evil-set-initial-state 'exwm-mode 'motion))


	;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-\ ;; Ctrl+Space
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:))  

	;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
	
	(setq exwm-input-global-keys
				`(
					([?\s-r] . exwm-reset)

					([?\s-e] . perspective-exwm-switch-perspective)

					([?\s-w] . kill-current-buffer)

					([?\s-W] . delete-window) 

					([?\s-Q] . kill-emacs)

					;; Window Movement
					([?\s-h] . windmove-left)
					([?\s-j] . windmove-down)
					([?\s-k] . windmove-up)
					([?\s-l] . windmove-right)

					([?\s-H] . buf-move-left)
					([?\s-J] . buf-move-down)
					([?\s-K] . buf-move-up)
					([?\s-L] . buf-move-right)


					;; Create Window Frames
					([?\s-i] . split-window-vertically)
					([?\s-o] . split-window-horizontally)
					
					,@(mapcar (lambda (i)
											`(,(kbd (format "s-%d" i)) .
												(lambda ()
													(interactive)
													(exwm-workspace-switch-create ,i))))
										(number-sequence 0 9))))



	
	(exwm-enable) ;; Must be at the END
	)
