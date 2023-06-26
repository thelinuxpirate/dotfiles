;;; elisp.el - Custom Elisp Code For GNU Emacs

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
			 (persp-switch "1")
			 (setq zonai/current-workspace "1"))

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

;; Resize Windows
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

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
