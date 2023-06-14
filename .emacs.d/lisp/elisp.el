;;; elisp.el - Custom Elisp Code

;; This file is NOT part of GNU Emacs.

;;; Code

;; A function that switches to GOD-MODE and disables EVIL-Mode.
(defun evil-god-mode ()
  "Switch to GOD-MODE & disable Evil-Mode"
  (interactive)
  (turn-off-evil-mode)
  (god-mode-all))

(defun evil-god-local-mode ()
  "Switch to GOD-LOCAL-MODE & disable Evil-Mode"
  (interactive)
  (turn-off-evil-mode)
  (god-local-mode))
