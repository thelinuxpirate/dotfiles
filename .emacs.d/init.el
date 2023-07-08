;; Startup Elpaca Package Manager
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;; Important Package to keep ~/.emacs.d/ clean
(use-package no-littering
  :demand t
  :config
  (setq no-littering-etc-directory
      (expand-file-name ".config/" user-emacs-directory))
  (setq no-littering-var-directory
      (expand-file-name ".data/" user-emacs-directory)))

;; Init
(set-default-coding-systems 'utf-8)
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(load "~/.emacs.d/lisp/elisp.el")
(load "~/.emacs.d/lisp/home.el")
(setq-default cursor-in-non-selected-windows nil)
;;(load "~/.emacs.d/lisp/linux.el")

;; Looks & Fonts
(use-package doom-themes
	:demand t
	:init (load-theme 'doom-palenight t)	
	:config
	(setq doom-themes-enable-bold t    
        doom-themes-enable-italic t))

(use-package nord-theme :demand t) ;; Backup Theme
(use-package ivy :demand t :init (ivy-mode))
(use-package projectile :demand t)

(use-package dashboard
	:demand t
	:after projectile
	:config
	(dashboard-setup-startup-hook)
	(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
	;; Set the title
	(setq dashboard-banner-logo-title "Welcome to Zonaimacs")
	;; Set the banner
	(setq dashboard-startup-banner "~/.emacs.d/.custom/.dashboard_logos/shine.txt")

	;; Content is not centered by default. To center, set
	(setq dashboard-center-content t)
	(setq dashboard-show-shortcuts t)

	(setq dashboard-items '((recents  . 5)
													(bookmarks . 5)
													(projects . 5)
													(agenda . 5)
													(registers . 5)))

	(setq dashboard-icon-type 'all-the-icons) 
	(setq dashboard-display-icons-p t)
	(setq dashboard-icon-type 'nerd-icons)
	
	(setq dashboard-set-navigator t)
	(setq dashboard-set-init-info t)

	(setq dashboard-set-init-info t)
	(setq dashboard-set-footer t)
	(setq dashboard-footer-messages '("\"Emacs is just a text editor\" - Some Windows User"))

	(setq dashboard-week-agenda t) ;; Org Agenda
	(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda))

(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.2)
	:init (which-key-mode))
(use-package beacon :demand t :init (beacon-mode))

(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 6)

  (doom-modeline-env-version t)
  (doom-modeline-hud t)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

(use-package rainbow-mode :demand t :config (add-hook 'prog-mode-hook (lambda () (rainbow-mode))))
;; all-the-icons-install-fonts
(use-package all-the-icons :demand t :if (display-graphic-p))
;; nerd-icons-install-fonts
(use-package nerd-icons :demand t)
(use-package treemacs-all-the-icons :demand t :config (treemacs-load-theme "all-the-icons"))

;; Utilities & Misc
(use-package vterm :demand t)
(use-package magit :demand t)

(use-package perspective
	:demand t
	:custom
	(persp-mode-prefix-key (kbd "C-."))
	(persp-initial-frame-name "1")
	:init (persp-mode))

(use-package treemacs :demand t)
(use-package ranger
	:demand t
	:config
	(setq ranger-cleanup-eagerly t)
	(setq ranger-modify-header t)
	(setq ranger-show-hidden t))

(use-package multiple-cursors :demand t)
(use-package sudo-edit :demand t)
(use-package sudo-utils :demand t)
(use-package elcord :demand t :init (elcord-mode)) ;; Discord Status of Emacs

;; Syntax & LSP
(use-package tree-sitter :demand t :init (global-tree-sitter-mode))
(use-package tree-sitter-langs :demand t)

(use-package lsp-mode
	:demand t
	:init (setq lsp-keymap-prefix "C-c l")
	:hook
	(prog-mode-hook . lsp)
	(lsp-mode . lsp-enable-which-key-integration)
	:commands lsp)
(use-package lsp-ui :demand t :commands lsp-ui-mode) ;; Extra LSP Packages
(use-package lsp-ivy :demand t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :demand t :commands lsp-treemacs-errors-list)
(use-package dap-mode :demand t)
(use-package company :demand t :config (add-hook 'prog-mode-hook #'global-company-mode))

;; Language Modes
(use-package paredit
	:demand t
	:init (autoload 'enable-paredit-mode "paredit" t)
	:hook
	(emacs-lisp-mode-hook . enable-paredit-mode)
  (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
  (ielm-mode-hook . enable-paredit-mode)
  (lisp-mode-hook . enable-paredit-mode)
  (lisp-interaction-mode-hook . enable-paredit-mode)
  (scheme-mode-hook . enable-paredit-mode)
	(yuck-mode-hook . enable-paredit-mode))

(use-package geiser :demand t)
(use-package geiser-guile :demand t)
(use-package nix-mode :demand t)
;; Install rust-analyzer for lsp
(use-package rust-mode :demand t :hook (rust-mode-hook . cargo-minor-mode))
(use-package cargo :demand t)

(use-package go-mode :demand t)
(use-package haskell-mode :demand t)
(use-package zig-mode :demand t) ;; Install zls for lsp

(use-package typescript-mode :demand t)
(use-package kotlin-mode :demand t)
(use-package yuck-mode :demand t)

(use-package go-translate ;; Helps translating when modding games
	:demand t
	:config
	(setq gts-translate-list '(("en" "ja") ("en" "es")))

	(setq gts-default-translator
				(gts-translator
				 :picker (gts-prompt-picker)
				 :engines (list (gts-bing-engine) (gts-google-engine))
				 :render (gts-buffer-render))))

;; Keybindings/Mappings
(use-package general
  :demand t
  :config
  (general-evil-setup)
	(setq evil-want-keybinding nil)

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

  ;; God Mode Setup
  (general-create-definer zonai/GOD
    :keymaps 'override)

	;; EXWM Setup
	(general-create-definer zonai/exwm
		:states 'normal
    :keymaps 'override) 
	
  (zonai/leader-mappings-norm
		;; BUFFER MANAGEMENT
    "j"       '(:ignore t                 :wk "Buffer KeyChords")
    "j s"     '(switch-to-buffer          :wk "Switch to an Active Buffer")
    "j r"     '(revert-buffer             :wk "Reload Current Buffer")
    "j k"     '(kill-current-buffer       :wk "Kills Current Buffer")
    "j f"     '(ibuffer-list-buffers      :wk "List Buffers")
    "j <tab>" '(switch-to-prev-buffer     :wk "Switch to Previous Buffer")
    "j SPC"   '(switch-to-next-buffer     :wk "Switch to Next Buffer")

		;; GOD MODE SETTINGS
    "g"       '(:ignore t                 :wk "GOD MODE MAPPINGS")
    "g g"     '(zonai/evil-god-mode-all   :wk "SWITCH TO GOD MODE GLOBAL")
    "g l"     '(zonai/evil-god-local-mode :wk "SWITCH TO GOD MODE BUFFER")
    "g j"     '(evil-execute-in-god-state :wk "EXECUTE CMD IN GOD STATE")
    "g ?"     '(zonai/god-mode-manual     :wk "OPEN GOD MODE MANUAL")

		;; Root
		"s"       '(:ignore t                 :wk "Options as Root")
		"s e"     '(sudo-edit                 :wk "Open Current File as Root")
		"s f"     '(sudo-edit-find-file       :wk "Find File as Root")

		;; MISC
    "f"       '(find-file                 :wk "Find & Open File")
		"t"       '(vterm                     :wk "Launch VTerm"))

  (zonai/localleader-mappings-norm
		;; WINDOW MANAGEMENT
		"s"   '(:ignore t                 :wk "Split Windows Prefix")
		"s s" '(split-window-vertically   :wk "Split Window Vertically")
    "s h" '(split-window-horizontally :wk "Split Window Horizontally")

    "h"   '(windmove-left             :wk "Move Window Focus to the Left")
    "j"   '(windmove-down             :wk "Move Window Focus to the Down")
    "k"   '(windmove-up               :wk "Move Window Focus to the Up")
    "l"   '(windmove-right            :wk "Move Window Focus to the Right")

    "s k" '(delete-window             :wk "Delete Current Window")

		;; Misc
		"t" '(treemacs                    :wk "Toggle Treemacs")
		"c" '(cargo-minor-mode-command-map :wk "Show Cargo Commands")
		
		;; Workspaces/Persp-Mode
		"<tab>"   '(:ignore t    :wk "Workspaces")

		"<tab> 1" '(zonai/switch-to-workspace-01 :wk "Switch to Main Workspace")
		"<tab> 2" '(zonai/switch-to-workspace-02 :wk "Switch to Workspace 2")
		"<tab> 3" '(zonai/switch-to-workspace-03 :wk "Switch to Workspace 3")
		"<tab> 4" '(zonai/switch-to-workspace-04 :wk "Switch to Workspace 4")
		"<tab> 5" '(zonai/switch-to-workspace-05 :wk "Switch to Workspace 5")
		"<tab> 6" '(zonai/switch-to-workspace-06 :wk "Switch to Workspace 6")
		"<tab> 7" '(zonai/switch-to-workspace-07 :wk "Switch to Workspace 7")
		"<tab> 8" '(zonai/switch-to-workspace-08 :wk "Switch to Workspace 8")
		"<tab> 9" '(zonai/switch-to-workspace-09 :wk "Switch to Workspace 9")
		"<tab> 0" '(zonai/switch-to-workspace-10 :wk "Switch to Workspace 10")

    "<tab> k" '(persp-next   :wk "Switch to Next Workspace")
		"<tab> j" '(persp-prev   :wk "Switch to Previous Workspace")
		"<tab> q" '(persp-kill   :wk "Kill Current Workspace"))

	(zonai/GOD
		;; Movement
		"C-1" '(backward-char             :wk "Move Backward")
		"C-2" '(next-line                 :wk "Move Down")
		"C-3" '(forward-char              :wk "Move Foward")
		"C-o" '(previous-line             :wk "Move Up")

		"C-4" '(move-end-of-line          :wk "Move to the End of the Line")
		"C-`" '(move-beginning-of-line    :wk "Move to the Start of the Line")
		
		"C-x" '(:ignore t                 :wk "Action Key Prefix")
		"C-W" '(move-beginning-of-line    :wk "Move to the Start of the Line")

		"C-?" '(zonai/god-mode-manual    :wk "Opens God-Mode Manual")
		"C-;" '(zonai/become-human       :wk "Return to Human State")))

(use-package evil
  :demand t
  :init (evil-mode)
  :config
  (setq-default tab-width 2)
  (setq-default evil-shift-width tab-width)

  (evil-define-key 'insert 'global (kbd "M-e") 'evil-normal-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
  (evil-define-key 'normal 'global (kbd "P") 'vterm-yank))
;; Extra stuff for Evil
(use-package evil-god-state :demand t :after evil)
(use-package evil-collection :demand t :after evil)

(use-package god-mode
  :demand t
  :after evil
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (setq god-mode-enable-function-key-translation nil))

(use-package hydra :demand t)

;; Org Mode Configuration
(use-package org
  :demand t
	:init (org-mode)
  :config
  (evil-define-key 'normal 'global (kbd "<tab>") 'org-cycle)

  (setq org-startup-indented t)           
  (setq org-startup-with-inline-images t)
  (setq org-src-fontify-natively t)

  (custom-set-faces
   '(org-block-begin-line
     ((t (:underline "#292D3E" :foreground "#8fbcbb" :background "#2e3440" :extend t))))
   '(org-block
     ((t (:background "#3b4252" :extend t))))
   '(org-block-end-line
     ((t (:overline "#292D3E" :foreground "#8fbcbb" :background "#2e3440" :extend t))))))

(use-package org-roam
  :demand t
  :after org
	:config (add-hook 'org-mode-hook (lambda () (org-roam-mode))))

(use-package org-superstar 
  :demand t
  :after org-roam
	:config (add-hook 'org-mode-hook (lambda () (org-superstar-mode))))

(use-package org-present :demand t :after org-roam)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elcord-editor-icon "emacs_material_icon")
 '(elcord-idle-message "Compiling Software...")
 '(elcord-idle-timer 500)
 '(elcord-quiet t)
 '(elcord-refresh-rate 1)
 '(warning-suppress-log-types
	 '((org-element-cache)
		 (org-element-cache)
		 (org-element-cache)))
 '(warning-suppress-types '((org-element-cache) (org-element-cache))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#3b4252" :extend t))))
 '(org-block-begin-line ((t (:underline "#292D3E" :foreground "#8fbcbb" :background "#2e3440" :extend t))))
 '(org-block-end-line ((t (:overline "#292D3E" :foreground "#8fbcbb" :background "#2e3440" :extend t)))))
