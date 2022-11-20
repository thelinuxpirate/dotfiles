(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 7)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 0x0 --rotate normal")))
  (exwm-randr-enable)

  ;; IF I want to use a system tray (deafult is disabled): 
  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs;
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  ;;(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-l] . windmove-left)
          ([s-h] . windmove-right)
          ([s-k] . windmove-up)
          ([s-j] . windmove-down)
	  ([?\s-q] . kill-emacs)

          ;; Launch applications via shell command
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

         ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
         ,@(mapcar (lambda (i)
                     `(,(kbd (format "s-%d" i)) .
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
                   (number-sequence 0 6))))

 (exwm-enable))
