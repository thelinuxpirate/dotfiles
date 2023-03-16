(require 'evil)
(evil-set-leader 'normal (kbd "SPC")) ;; Defines the leader key to be the <SPACE> key;
;; MISC:

;; LEADER + TAB (The main keycord, the C-x of this config):
(evil-define-key 'normal 'global (kbd "<leader><tab><deletechar>") 'backward-kill-sentence)
(evil-define-key 'normal 'global (kbd "<leader><tab>#") 'server-edit)
(evil-define-key 'normal 'global (kbd "<leader><tab>$") 'set-selective-display)
(evil-define-key 'normal 'global (kbd "<leader><tab>'") 'expand-abbrev)
(evil-define-key 'normal 'global (kbd "<leader><tab>*") 'calc-dispatch)
(evil-define-key 'normal 'global (kbd "<leader><tab>+") 'balance-windows)
(evil-define-key 'normal 'global (kbd "<leader><tab>-") 'shrink-window-if-larger-than-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>.") 'set-fill-prefix)
(evil-define-key 'normal 'global (kbd "<leader><tab>e") 'counsel-M-x)
(evil-define-key 'normal 'global (kbd "<leader><tab>k") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>s") 'vterm)
(evil-define-key 'normal 'global (kbd "<leader><tab>b") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader><tab>d f") 'counsel-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>l") 'load-file)
(evil-define-key 'normal 'global (kbd "<leader><tab>f") 'counsel-find-file)
(evil-define-key 'normal 'global (kbd "<leader><tab> <tab>") 'switch-to-prev-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab> SPC") 'switch-to-next-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>c") 'comment-region)
(evil-define-key 'normal 'global (kbd "<leader><tab>d c") 'uncomment-region)
(evil-define-key 'normal 'global (kbd "<leader><tab>d c") 'uncomment-region)
(evil-define-key 'normal 'global (kbd "<leader><tab>d c") 'uncomment-region)

;; LEADER + O (Contains Org Mode Related Bindings):
(evil-define-key 'normal 'visual (kbd "<leader>o l") 'org-insert-link)
(evil-define-key 'normal 'global (kbd "<leader>o df") 'org-babel-tangle)

;; LEADER + W (Contains Bindings Related to Windows & Buffers):
(evil-define-key 'normal 'global (kbd "<leader>w w") 'make-frame)
(evil-define-key 'normal 'global (kbd "<leader>w 1") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>w 2") 'delete-other-windows)
(evil-define-key 'normal 'global (kbd "<leader>w 3") 'split-window-below)
(evil-define-key 'normal 'global (kbd "<leader>w 4") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>w 5") 'split-window-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>w 6") 'split-window-vertically)

;; LEADER + T (Vterm Keybinds):
(evil-define-key 'normal 'global (kbd "<leader>t j") 'vterm-yank)
(evil-define-key 'normal 'global (kbd "<leader>t k") 'vterm-send-stop)
;; LEADER + Cord:
(evil-define-key 'normal 'global (kbd "<leader>j k") 'kill-current-buffer)
