;;; elisp.el - Custom Elisp Code

;; This file is NOT part of GNU Emacs.

;;; Code

;; A function that switches to GOD-MODE and disables EVIL-Mode.
(defun zonai/switch-workspace () (interactive)
	(persp-switch "2"))

(defun zonai/evil-delete () (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after))
	(setq-default evil-kill-on-visual-paste nil))

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
