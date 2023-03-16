(menu-bar-mode -1)
(scroll-bar-mode -1)        
(tool-bar-mode -1)          
(tooltip-mode -1) 
(display-battery-mode -1) ;; Disabled on DWM;
(display-time-mode -1) ;; Disabled on DWM;
(global-display-line-numbers-mode 1)
(set-fringe-mode 10)
(load-file "~/.emacs.d/keybinds/emacs-bindings.el") 
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nord t) ;; or dracula
(load-file "~/.emacs.d/packages/package-repositories.el")

(load-file "~/.emacs.d/package.el")
(require 'use-package)

(load-file "~/.emacs.d/keybinds/evil-bindings.el")
(use-package evil
  :demand t
  :init
  :config
(evil-mode 1)
(custom-set-variables
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(package-selected-packages
   '(dashboard all-the-icons projectile page-break-lines use-package desktop-environment 
   exwm tree-sitter-langs tree-sitter vterm doom-modeline go-mode rust-mode evil cmake-mode)))
(custom-set-faces))

(use-package org)

(load-file "~/.emacs.d/packages/beacon.el")
(beacon-mode 1)

(load-file "~/.emacs.d/packages/whichkey.el")
(which-key-mode 1)
(load-file "~/.emacs.d/packages/doom-modeline.el")

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(page-break-lines-mode 1)
(projectile-mode 1)
(use-package all-the-icons
  :if (display-graphic-p))
(load-file "~/.emacs.d/packages/dashboard.el")
(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook))
    
;;(load-file "~/.emacs.d/exwm/exwm_config.el") ;; Disable if not using Exwm;
;;(exwm-enable)

(use-package counsel ;; Counsel is the pkg name, MELPA, 2 or three depend all-the-icons-ivy, all-the-icons-dried
  :custom
    (counsel-linux-app-format-function#'counsel-linux-app-format-function-name-only))
(ivy-mode 1)

;; Make it look better (MELPA, ivy-rich, all-the-icons-ivy-rich):
(use-package ivy-rich
  :init
(ivy-rich-mode 1)
  :config
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package highlight-parentheses ;; MELPA PKG: highlight-parentheses
  :ensure t)
(global-highlight-parentheses-mode)

(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode 1)

(use-package rust-mode)

(use-package go-mode)

(use-package yaml-mode)

(use-package haskell-mode)

(use-package rainbow-mode)
(rainbow-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(package-selected-packages
   '(org-modern dashboard all-the-icons projectile page-break-lines use-package desktop-environment exwm tree-sitter-langs tree-sitter vterm doom-modeline go-mode rust-mode evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
