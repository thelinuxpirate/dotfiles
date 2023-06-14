;; Startup Elpaca Package Manager
(defvar elpaca-installer-version 0.4)
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
(add-hook 'prog-mode-hook 'global-company-mode 1)
(load "~/.emacs.d/lisp/elisp.el")

;; Looks & Fonts
(use-package nord-theme
  :demand t
  :init (load-theme 'nord t))

(use-package ivy
  :demand t
  :init (ivy-mode 1))

(use-package which-key
  :demand t
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.2))

(use-package beacon
  :demand t
  :init (beacon-mode 1))

(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 6)

  (doom-modeline-env-version t)
  (doom-modeline-hud t)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-minor-modes t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

(use-package rainbow-mode
  :demand t
  :init (rainbow-mode 1))

;; all-the-icons-install-fonts
(use-package all-the-icons
  :demand t
  :if (display-graphic-p))

;; nerd-icons-install-fonts
(use-package nerd-icons
  :demand t)

;; Utilities & Misc
(use-package vterm
  :demand t)

(use-package magit
  :demand t)

(use-package perspective
	:demand t
	:custom
	(persp-mode-prefix-key (kbd "C-."))
	(persp-initial-frame-name "Main")
	(persp-switch "neigh")
	:init (persp-mode 1))

(use-package elcord
  :demand t
  :init (elcord-mode 1))

;; Syntax & LSP
(use-package tree-sitter
  :demand t
	:init (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
	:demand t)

(use-package eglot
  :demand t
  :mode "\\.rs\\'")

(use-package company
  :demand t)

(use-package rust-mode
  :demand t
  :config
  (add-hook 'rust-mode-hook
    (lambda () (setq indent-tabs-mode nil))))

;; Keybindings/Mappings
(use-package general
  :demand t
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

  ;; God Mode Setup
  (general-create-definer zonai/GOD
    :keymaps 'override)

  (zonai/GOD
   "?"   '(zonai/god-mode-manual          :wk "Opens God-Mode Manual")
   "C-'" '(zonai/become-human             :wk "Return to Human State"))

  
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
    "g g"     '(zonai/evil-god-mode-all   :wk "SWITCH TO GOD MODE GLOBAL")
    "g l"     '(zonai/evil-god-local-mode :wk "SWITCH TO GOD MODE BUFFER")
    "g j"     '(evil-execute-in-god-state :wk "EXECUTE CMD IN GOD STATE")
    "g ?"     '(zonai/god-mode-manual     :wk "OPEN GOD MODE MANUAL")

   ;; MISC
    "f"       '(find-file                 :wk "Find & Open File"))

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

	 ;; Workspaces/Persp-Mode
		"<tab>"   '(:ignore t    :wk "Workspaces")
    "<tab> j" '(zonai/switch-workspace :wk "Switch Workspace")
    "<tab> l" '(persp-next   :wk "Switch to Next Workspace")
		"<tab> h" '(persp-prev   :wk "Switch to Previous Workspace") 


    )



  )

(use-package evil
  :demand t
  :init (evil-mode 1)
  :config
  (setq-default tab-width 2)
  (setq-default evil-shift-width tab-width)

  (evil-define-key 'insert 'global (kbd "M-e") 'evil-normal-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

(use-package evil-god-state
  :demand t
  :after evil)

(use-package god-mode
  :demand t
  :after evil
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (setq god-mode-enable-function-key-translation nil))

(use-package hydra
  :demand t)

;; Org Mode Configuration
(use-package org
  :demand t
  :config
  (evil-define-key 'normal 'global (kbd "<tab>") 'org-cycle)

  (setq org-startup-indented t)           
  (setq org-startup-with-inline-images t)
  (setq org-src-fontify-natively t)

  (custom-set-faces
   '(org-block-begin-line
     ((t (:underline "#2e3440" :foreground "#8fbcbb" :background "#2e3440" :extend t))))
   '(org-block
     ((t (:background "#3b4252" :extend t))))
   '(org-block-end-line
     ((t (:overline "#2e3440" :foreground "#8fbcbb" :background "#2e3440" :extend t))))))

(use-package org-roam
  :demand t
  :after org)

(use-package org-superstar 
  :demand t
  :after org-roam
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-present
  :demand t
  :after org-roam)
