;;; elisp.el - Custom Elisp Code

;; This file is NOT part of GNU Emacs.

;;; Code

;; A function that switches to GOD-MODE and disables EVIL-Mode.
 ;; Evil MODE \/ GOD MODE
(defun zonai/evil-god-mode-all () (interactive)
  (turn-off-evil-mode)
  (god-mode-all)
  (which-key-enable-god-mode-support)
  (message "GOD MODE ACTIVATED; (kbd \"?\") for help!"))

(defun zonai/evil-god-local-mode () (interactive)
  (turn-off-evil-mode)
  (god-local-mode)
  (which-key-enable-god-mode-support)
  (message "GOD MODE ACTIVATED LOCAL; (kbd \"?\") for help!"))

(defun zonai/become-human () (interactive)
  (god-mode)
  (turn-on-evil-mode)
  (message "You have returned to your Human State!"))

(defun zonai/god-mode-manual () (interactive)
  (get-buffer-create "Zonai/God-Mode-Manual")
  (switch-to-buffer "Zonai/God-Mode-Manual") 


  (find-file "~/.emacs.d/.custom/GOD-MODE-Manual.org")
  (org-present)
  (turn-off-evil-mode)

  (message "Here is the Manual!")
  (read-only-mode)) 

 ;; Workspaces
(defvar zonai/current-workspace)

(defun zonai/switch-to-workspace-01 () (interactive)
			 (persp-switch "Main")
			 (setq zonai/current-workspace "Main"))

(defun zonai/switch-to-workspace-02 () (interactive)
			 (persp-switch "[2]")
			 (setq zonai/current-workspace "[2]"))

(defun zonai/switch-to-workspace-03 () (interactive)
			 (persp-switch "[3]")
			 (setq zonai/current-workspace "[3]"))

(defun zonai/switch-to-workspace-04 () (interactive)
			 (persp-switch "[4]")
			 (setq zonai/current-workspace "[4]"))

(defun zonai/switch-to-workspace-05 () (interactive)
			 (persp-switch "[5]")
			 (setq zonai/current-workspace "[5]"))

(defun zonai/switch-to-workspace-06 () (interactive)
			 (persp-switch "[6]")
			 (setq zonai/current-workspace "[6]"))

(defun zonai/switch-to-workspace-07 () (interactive)
			 (persp-switch "[7]")
			 (setq zonai/current-workspace "[7]"))

(defun zonai/switch-to-workspace-08 () (interactive)
			 (persp-switch "[8]")
			 (setq zonai/current-workspace "[8]"))

(defun zonai/switch-to-workspace-09 () (interactive)
			 (persp-switch "[9]")
			 (setq zonai/current-workspace "[9]"))

(defun zonai/switch-to-workspace-10 () (interactive)
			 (persp-switch "[10]")
			 (setq zonai/current-workspace "[10]"))

(defun zonai/kill-current-workspace () (interactive)
			 (persp-kill zonai/current-workspace))

;; Misc \/ Other
(defun zonai/evil-delete () (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after))
	(setq-default evil-kill-on-visual-paste nil))

(defun zonai/repeat-command (command)
  "Repeat COMMAND."
  (require 'repeat)       ; Define its vars before we let-bind them.
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           #'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))
