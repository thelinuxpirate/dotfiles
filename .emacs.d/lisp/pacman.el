;; Functions for Linux Options via 'Arch GNU/Linux (pacman, paru, systemd)
;; This file is NOT part of GNU Emacs.

(defgroup linux-sys nil
  "Customization options for linux settings"
  :group 'help
  :prefix "linux-")

(defcustom linux-popup-type 'side-window
  "Supported types are minibuffer, side-window, frame, and custom"
  :group 'linux-sys
  :type '(radio (const :tag "Appear in a MiniBuffer"   minibuffer)
                (const :tag "Appear in a Side-Window"  side-window)
                (const :tag "Popup Frame"              frame)
                (const :tag "Custom Display Functions" custom)))

(defcustom root-perm nil
	"If Root Permission is ok"
	:group 'linux-sys
	:type  'bool)


(defun pac-upgrade ()
	(interactive)
	(shell-command-on-region
	 (point-min) (point-max)
	 (read-shell-command "sudo pacman -Syu")))
