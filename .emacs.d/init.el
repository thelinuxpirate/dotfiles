;; Enables visible scrollbar:
(scroll-bar-mode 1)        
;; Disable the toolbar:
(tool-bar-mode -1)          
;; Disable tooltips:
(tooltip-mode -1) 
;; Cusor Color:
(set-cursor-color "#aa50d4") 

;; Enable Themes:
(add-to-list 'custom-theme-load-path "/home/thelinuxpirate/.emacs.d/themes")
(load-theme 'dracula t)

(load-file "/home/thelinuxpirate/.emacs.d/keybinds/evil-bindings.el") ;; Loads Keybindings set for EVIL Mode;

(load-file "/home/thelinuxpirate/.emacs.d/keybinds/emacs-bindings.el") ;; Loads Keybindings set for Emacs;

;; Enable Evil:
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "/home/thelinuxpirate/.emacs.d/packages/beacon.el") ;; Loads the beacon-mode package

;; Beacon Mode:
(beacon-mode 1)

(load-file "/home/thelinuxpirate/.emacs.d/packages/whichkey.el") ;; Loads the which key config

;; Which-Key:
(which-key-mode 1)

;; Rust-Mode:
(add-to-list 'load-path "/home/thelinuxpirate/.emacs.d/packages/rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
