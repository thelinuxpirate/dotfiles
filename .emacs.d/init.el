;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(load "~/.emacs.d/lisp/elisp.el")

;; Looks & Fonts
(use-package nord-theme
  :straight t
  :init (load-theme 'nord t))

;; all-the-icons-install-fonts
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; nerd-icons-install-fonts
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package beacon
  :straight t
  :init (beacon-mode 1))

(use-package ivy
  :straight t
  :init (ivy-mode 1))

(use-package org-superstar  ;; Improved version of org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package which-key
  :straight t
  :init (which-key-mode 1)
  :config
  (which-key-enable-god-mode-support))

;; Utilities & Misc
(use-package vterm
  :straight t)

(use-package magit
  :straight t)

(use-package elcord
  :straight t
  :init (elcord-mode 1))

;; Syntax & LSP
(use-package tree-sitter
  :straight t)

(use-package eglot
  :straight t
  :mode "\\.rs\\'")

(use-package company
  :straight t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rust-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook
    (lambda () (setq indent-tabs-mode nil))))

;; Keybindings/Mappings
(use-package general
  :straight t
  :config
  (general-evil-setup)

  ;; Leader Keys Setup 
  (general-create-definer zonai/leader-mappings-norm
    :states  'normal
    :keymaps 'override
    :prefix  ";") 

  (general-create-definer zonai/leader-mappings-ins
    :states  'insert
    :keymaps 'override
    :prefix  "M-;")

  ;; Local-Leader Key  
  (general-create-definer zonai/localleader-mappings-norm
    :states  'normal 
    :keymaps 'override
    :prefix  "SPC")

  (zonai/leader-mappings-norm
   ;; BUFFER MANAGEMENT
    "j"       '(:ignore t                 :wk "Buffer KeyChords")
    "j s"     '(switch-to-buffer          :wk "Switch to an Active Buffer")
    "j r"     '(revert-buffer             :wk "Reload Current Buffer")
    "j k"     '(kill-current-buffer       :wk "Kills Current Buffer")
    "j f"     '(ibuffer-list-buffers      :wk "List Buffers")
    "j <tab>" '(switch-to-prev-buffer     :wk "Switch to Previous Buffer")
    "j SPC"   '(switch-to-next-buffer     :wk "Switch to Next Buffer")

   ;; GOD MODE
    "g"       '(:ignore t                 :wk "GOD MODE MAPPINGS")
    "g g"     '(evil-god-mode-all         :wk "SWITCH TO GOD MODE GLOBAL")
    "g l"     '(evil-god-local-mode       :wk "SWITCH TO GOD MODE BUFFER")
    "g j"     '(evil-execute-in-god-state :wk "EXECUTE CMD IN GOD STATE")

   ;; MISC
    "f"       '(find-file                 :wk "Find & Open File"))

  (zonai/localleader-mappings-norm
   ;; WINDOW MANAGEMENT
    "s s" '(split-window-vertically   :wk "Split Window Vertically")
    "s h" '(split-window-horizontally :wk "Split Window Horizontally")

    "h"   '(windmove-left             :wk "Move Window Focus to the Left")
    "j"   '(windmove-down             :wk "Move Window Focus to the Down")
    "k"   '(windmove-up               :wk "Move Window Focus to the Up")
    "l"   '(windmove-right            :wk "Move Window Focus to the Right")

    "s k" '(delete-window             :wk "Delete Current Window")))

(use-package evil
  :straight t
  :init (evil-mode 1)
  :config
  (evil-define-key 'insert 'global (kbd "M-e") 'evil-normal-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

(use-package evil-god-state
  :straight t)

(use-package god-mode
  :straight t
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (setq god-mode-enable-function-key-translation nil))

(use-package hydra
  :straight t)

;; Org Mode Configuration
(use-package org
  :straight t
  :config
(setq org-startup-indented t)           ;; Indent according to section
(setq org-startup-with-inline-images t) ;; Display images in-buffer by default
(evil-define-key 'normal 'global (kbd "<tab>") 'org-cycle))
