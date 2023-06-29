;; Functions for Linux Options via 'Arch GNU/Linux (pacman, paru, systemd)
;; This file is NOT part of GNU Emacs.

;; This file is meant to control package management
;; via GNU Guix & Arch GNU/Linux's Pacman 
;; I use Arch GNU/Linux + GNU Guix Package Manager

;; TODO: Make functions to controll Linux System via Emacs

(use-package guix
	:demand t)

(defvar root-perm nil "Root Permission Variable")

(defun pac-upgrade ()
	(interactive)
	(shell-command-on-region
	 (point-min) (point-max)
	 (read-shell-command "sudo pacman -Syu")))
