;; -*-lisp-*-

;; Init:
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(set-module-dir "/usr/share/stupmwm/contrib/") ;; AUR Package PATH

;; Autostart:
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "xset r rate 200 60")
(run-shell-command "feh --bg-fill ~/.stumpwm.d/config/wm/wallpapers/p_emacs.png")

(run-shell-command "emacs --daemon &")
(run-shell-command "picom --daemon &")
(run-shell-command "dunst &")

;; Load Other Files
(load "~/.stumpwm.d/config/keybinds/bindings.lisp")

;; Activate the mode-line
(when *initializing*
  (mode-line))

;; Activate Modules
(load-module "globalwindows")

;; Startup Message
(setf *startup-message* "Welcome to StumpWM!")
