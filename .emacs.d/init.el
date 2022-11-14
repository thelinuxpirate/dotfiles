;; NOTE: This config uses a good amount of MELPA Packages so if you port this config to install those packages!:
;; https://github.com/DarthYoshi07/dotfiles <- The name DarthYoshi is an elden name I made when I was younger so I
;; hope to gain credit as "TheLinuxPirate";

;; Disables the Menu at the top:
(menu-bar-mode -1)
;; Disables visible scrollbar:
(scroll-bar-mode -1)        
;; Disable the toolbar:
(tool-bar-mode -1)          
;; Disable tooltips:
(tooltip-mode -1) 
;; Cusor Color:
(set-cursor-color "#aa50d4") 
;; Display Line Numbers to the Left:
(global-display-line-numbers-mode 1)
;; Loads Keybindings set for Emacs;
(load-file "/home/thelinuxpirate/.emacs.d/keybinds/emacs-bindings.el") 

;; Enable Themes:
(add-to-list 'custom-theme-load-path "/home/thelinuxpirate/.emacs.d/themes")
(load-theme 'dracula t)

;; Loads Package Repositories:
(load-file "/home/thelinuxpirate/.emacs.d/packages/package-repositories.el")

;; Enable Evil:
(load-file "/home/thelinuxpirate/.emacs.d/keybinds/evil-bindings.el") ;; Loads Keybindings set for EVIL Mode;
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(package-selected-packages '(vterm doom-modeline go-mode rust-mode evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Beacon Mode (Instaled via Source, IF MELPA, Package Name: beacon-mode):
(load-file "/home/thelinuxpirate/.emacs.d/packages/beacon.el") ;; Loads the beacon-mode package
(beacon-mode 1)

;; Which-Key (Installed via Source, IF MELPA, Package Name: which-key):
(load-file "/home/thelinuxpirate/.emacs.d/packages/whichkey.el") ;; Loads the which key config
(which-key-mode 1)

;; DOOM Modeline (Installed via MELPA, Package Name: doom-modeline):
(load-file "/home/thelinuxpirate/.emacs.d/packages/doom-modeline.el")
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Syntax Packages start here:

;; Rust-Mode: (Installed via MELPA, Package Name: rust-mode)
(require 'rust-mode)

;; Go-Mode (Installed via MELPA, Package Name: go-mode):
(require 'go-mode)
