(require 'dbus)

(defgroup desktop-environment nil
  :group 'environment)


(defcustom desktop-environment-keyboard-backlight-normal-increment 1
  :type 'integer)

(defcustom desktop-environment-keyboard-backlight-normal-decrement -1
  :type 'integer)



(defcustom desktop-environment-brightness-normal-increment "2%+"
  :type 'string)

(defcustom desktop-environment-brightness-normal-decrement "2%-"
  :type 'string)

(defcustom desktop-environment-brightness-small-increment "1%+"
  :type 'string)

(defcustom desktop-environment-brightness-small-decrement "1%-"
  :type 'string)

(defcustom desktop-environment-brightness-get-command "brightnessctl"
  :type 'string)

(defcustom desktop-environment-brightness-get-regexp "\\([0-9]+%\\)"
  :type 'regexp)

(defcustom desktop-environment-brightness-set-command "brightnessctl set %s"
  :type 'string)



(defcustom desktop-environment-volume-normal-increment "2%+"
  :type 'string)

(defcustom desktop-environment-volume-normal-decrement "2%-"
  :type 'string)

(defcustom desktop-environment-volume-small-increment "1%+"
  :type 'string)

(defcustom desktop-environment-volume-small-decrement "1%-"
  :type 'string)

(defcustom desktop-environment-volume-get-command "amixer get Master"
  :type 'string)

(defcustom desktop-environment-volume-get-regexp "\\([0-9]+%\\)"
  :type 'regexp)

(defcustom desktop-environment-volume-set-command "amixer set Master %s"
  :type 'string)

(defcustom desktop-environment-volume-toggle-command "amixer set Master toggle"
  :type 'string)

(defcustom desktop-environment-volume-toggle-microphone-command "amixer set Capture toggle"
  :type 'string)



(defcustom desktop-environment-screenshot-command "scrot"
  :type 'string)

(defcustom desktop-environment-screenshot-partial-command "scrot -s"
  :type 'string)

(defcustom desktop-environment-screenshot-delay-argument "--delay %d"
  :type 'string)

(defcustom desktop-environment-screenshot-directory "~/Pictures"
  :type 'directory)



(defcustom desktop-environment-screenlock-command "slock"
  :type 'string)



(defcustom desktop-environment-wifi-command "wifi toggle"
  :type 'string)


(defcustom desktop-environment-bluetooth-command "bluetooth toggle"
  :type 'string)



(defcustom desktop-environment-music-toggle-command "playerctl play-pause"
  :type 'string)

(defcustom desktop-environment-music-previous-command "playerctl previous"
  :type 'string)

(defcustom desktop-environment-music-next-command "playerctl next"
  :type 'string)

(defcustom desktop-environment-music-stop-command "playerctl stop"
  :type 'string)



(defcustom desktop-environment-update-exwm-global-keys :global
  :type '(radio
          (const :tag "Global" :doc "Use `exwm-input-set-key' on mode activation to set bindings." :global)
          (const :tag "Prefix" :doc "Add/Remove keys to `exwm-input-prefix-keys' when enabling/disabling the mode." :prefix)
          (const :tag "Off" :doc "Do not touch EXWM key bindings." nil)))


(defun desktop-environment--shell-command-to-string (command)
   (let ((default-directory temporary-file-directory))
      (shell-command-to-string command)))

(defun desktop-environment-brightness-get ()
  (let ((output (desktop-environment--shell-command-to-string desktop-environment-brightness-get-command)))
    (save-match-data
      (string-match desktop-environment-brightness-get-regexp output)
      (match-string 1 output))))

(defun desktop-environment-brightness-set (value)
  (desktop-environment--shell-command-to-string (format desktop-environment-brightness-set-command value))
  (message "New brightness value: %s" (desktop-environment-brightness-get)))



(defun desktop-environment-volume-get ()
  (let ((output (desktop-environment--shell-command-to-string desktop-environment-volume-get-command)))
    (save-match-data
      (string-match desktop-environment-volume-get-regexp output)
      (match-string 1 output))))

(defun desktop-environment-volume-set (value)
  (desktop-environment--shell-command-to-string (format desktop-environment-volume-set-command value))
  (message "New volume value: %s" (desktop-environment-volume-get)))


(defun desktop-environment-keyboard-backlight-percent ()
  (let ((backlight-level (desktop-environment-keyboard-backlight-get)))
    (if (eq backlight-level 0)
        "0.0"
      (*
       (/ (* backlight-level 1.0)
          (* (desktop-environment-keyboard-backlight-get-max) 1.0))
       100))))

(defun desktop-environment-keyboard-backlight-get ()
  (dbus-call-method :system
                    "org.freedesktop.UPower"
                    "/org/freedesktop/UPower/KbdBacklight"
                    "org.freedesktop.UPower.KbdBacklight"
                    "GetBrightness"))

(defun desktop-environment-keyboard-backlight-get-max ()
  (dbus-call-method :system
                    "org.freedesktop.UPower"
                    "/org/freedesktop/UPower/KbdBacklight"
                    "org.freedesktop.UPower.KbdBacklight"
                    "GetMaxBrightness"))

(defun desktop-environment-keyboard-backlight-set (value)
  (dbus-call-method :system
                    "org.freedesktop.UPower"
                    "/org/freedesktop/UPower/KbdBacklight"
                    "org.freedesktop.UPower.KbdBacklight"
                    "SetBrightness"
                    :int32 value)
  (message "New keyboard value: %s%%" (desktop-environment-keyboard-backlight-percent)))



;;;###autoload
(defun desktop-environment-brightness-increment ()
  (interactive)
  (desktop-environment-brightness-set desktop-environment-brightness-normal-increment))

;;;###autoload
(defun desktop-environment-brightness-decrement ()
  (interactive)
  (desktop-environment-brightness-set desktop-environment-brightness-normal-decrement))

;;;###autoload
(defun desktop-environment-brightness-increment-slowly ()
  (interactive)
  (desktop-environment-brightness-set desktop-environment-brightness-small-increment))

;;;###autoload
(defun desktop-environment-brightness-decrement-slowly ()
  (interactive)
  (desktop-environment-brightness-set desktop-environment-brightness-small-decrement))



;;;###autoload
(defun desktop-environment-volume-increment ()
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-normal-increment))

;;;###autoload
(defun desktop-environment-volume-decrement ()
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-normal-decrement))

;;;###autoload
(defun desktop-environment-volume-increment-slowly ()
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-small-increment))

;;;###autoload
(defun desktop-environment-volume-decrement-slowly ()
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-small-decrement))

;;;###autoload
(defun desktop-environment-toggle-mute ()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-volume-toggle-command)))

;;;###autoload
(defun desktop-environment-toggle-microphone-mute ()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-volume-toggle-microphone-command)))


;;;###autoload
(defun desktop-environment-keyboard-backlight-increment ()
  (interactive)
  (desktop-environment-keyboard-backlight-set
   (+ desktop-environment-keyboard-backlight-normal-increment
      (desktop-environment-keyboard-backlight-get))))

(defun desktop-environment-keyboard-backlight-decrement ()
  (interactive)
  (desktop-environment-keyboard-backlight-set
   (+ desktop-environment-keyboard-backlight-normal-decrement
      (desktop-environment-keyboard-backlight-get))))



;;;###autoload
(defun desktop-environment-screenshot (&optional delay)
  (interactive "P")
  (let ((default-directory (expand-file-name desktop-environment-screenshot-directory))
        (command (if (and delay
                          (numberp delay)
                          (> delay 0))
                     (concat desktop-environment-screenshot-command
                             " "
                             (format desktop-environment-screenshot-delay-argument delay))
                   desktop-environment-screenshot-command)))
    (start-process-shell-command "desktop-environment-screenshot" nil command)))

;;;###autoload
(defun desktop-environment-screenshot-part (&optional delay)
  (interactive "P")
  (let ((default-directory (expand-file-name desktop-environment-screenshot-directory))
        (command (if (and delay
                          (numberp delay)
                          (> delay 0))
                     (concat desktop-environment-screenshot-partial-command
                             " "
                             (format desktop-environment-screenshot-delay-argument delay))
                   desktop-environment-screenshot-partial-command)))
    (message "Please select the part of your screen to shoot.")
    (start-process-shell-command "desktop-environment-screenshot" nil command)))



;;;###autoload
(defun desktop-environment-lock-screen ()
  (interactive)
  (start-process-shell-command "lock" nil desktop-environment-screenlock-command))



;;;###autoload
(defun desktop-environment-toggle-wifi ()
  (interactive)
  (let ((async-shell-command-buffer 'new-buffer))
    (async-shell-command desktop-environment-wifi-command)))

;;;###autoload
(defun desktop-environment-toggle-bluetooth ()
  (interactive)
  (let ((async-shell-command-buffer 'new-buffer))
    (async-shell-command desktop-environment-bluetooth-command)))



(defun desktop-environment-toggle-music ()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-music-toggle-command)))

(defun desktop-environment-music-previous ()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-music-previous-command)))

(defun desktop-environment-music-next()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-music-next-command)))

(defun desktop-environment-music-stop ()
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string desktop-environment-music-stop-command)))

(defvar desktop-environment-mode-map
  (let ((desktop-environment--keybindings
         `(;; Brightness
           (,(kbd "<XF86MonBrightnessUp>") . ,(function desktop-environment-brightness-increment))
           (,(kbd "<XF86MonBrightnessDown>") . ,(function desktop-environment-brightness-decrement))
           (,(kbd "S-<XF86MonBrightnessUp>") . ,(function desktop-environment-brightness-increment-slowly))
           (,(kbd "S-<XF86MonBrightnessDown>") . ,(function desktop-environment-brightness-decrement-slowly))
           ;; Volume
           (,(kbd "<XF86AudioRaiseVolume>") . ,(function desktop-environment-volume-increment))
           (,(kbd "<XF86AudioLowerVolume>") . ,(function desktop-environment-volume-decrement))
           (,(kbd "S-<XF86AudioRaiseVolume>") . ,(function desktop-environment-volume-increment-slowly))
           (,(kbd "S-<XF86AudioLowerVolume>") . ,(function desktop-environment-volume-decrement-slowly))
           (,(kbd "<XF86AudioMute>") . ,(function desktop-environment-toggle-mute))
           (,(kbd "<XF86AudioMicMute>") . ,(function desktop-environment-toggle-microphone-mute))
           ;; Screenshot
           (,(kbd "S-<print>") . ,(function desktop-environment-screenshot-part))
           (,(kbd "<print>") . ,(function desktop-environment-screenshot))
           ;; Screen locking
           (,(kbd "s-l") . ,(function desktop-environment-lock-screen))
           (,(kbd "<XF86ScreenSaver>") . ,(function desktop-environment-lock-screen))
           ;; Wifi controls
           (,(kbd "<XF86WLAN>") . ,(function desktop-environment-toggle-wifi))
           ;; Bluetooth controls
           (,(kbd "<XF86Bluetooth>") . ,(function desktop-environment-toggle-bluetooth))
           ;; Music controls
           (,(kbd "<XF86AudioPlay>") . ,(function desktop-environment-toggle-music))
           (,(kbd "<XF86AudioPrev>") . ,(function desktop-environment-music-previous))
           (,(kbd "<XF86AudioNext>") . ,(function desktop-environment-music-next))
           (,(kbd "<XF86AudioStop>") . ,(function desktop-environment-music-stop))))
        (map (make-sparse-keymap)))
    (dolist (keybinding desktop-environment--keybindings)
      (define-key map (car keybinding) (cdr keybinding)))
    map)

(declare-function exwm-input-set-key "ext:exwm-input")

(defun desktop-environment-exwm-set-global-keybindings (enable)
  (when (featurep 'exwm-input)
    (cl-case desktop-environment-update-exwm-global-keys
      (:global
       (when enable
         (map-keymap (lambda (event definition)
                       (exwm-input-set-key (vector event) definition))
                     desktop-environment-mode-map)))
      (:prefix
       (when (boundp 'exwm-input-prefix-keys)
         (map-keymap (lambda (event definition)
                       (ignore definition)
                       (setq exwm-input-prefix-keys (if enable
                                                        (cons event exwm-input-prefix-keys)
                                                      (delq event exwm-input-prefix-keys))))
                     desktop-environment-mode-map)))
      ((nil) nil)
      (t
       (message "Ignoring unknown value %s for `desktop-environment-update-exwm-global-keys'"
                desktop-environment-update-exwm-global-keys)))))

;;;###autoload
(define-minor-mode desktop-environment-mode
  :global t
  :require 'desktop-environment
  :lighter " DE"
  (desktop-environment-exwm-set-global-keybindings desktop-environment-mode))

(provide 'desktop-environment)
