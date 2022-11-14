;; JUST A NOTE: I use the Evil bindings for everything and try not to use the base Emacs Mode/Keybindings as much as possible, lots of things
;; are redefined and remapped here as I intend to avoid using Ctrl and the default Emacs keybindings
;; so this is the meat of the keybind config ( All of this is to save my pinky finger :) );
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
(evil-define-key 'normal 'global (kbd "<leader><tab>1") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader><tab>2") 'delete-other-windows)
(evil-define-key 'normal 'global (kbd "<leader><tab>3") 'split-window-below)
(evil-define-key 'normal 'global (kbd "<leader><tab>4") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader><tab>5") 'split-window-horizontally)
(evil-define-key 'normal 'global (kbd "<leader><tab>6") 'split-window-vertically)
(evil-define-key 'normal 'global (kbd "<leader><tab>e") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "<leader><tab>k") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>s") 'vterm)
(evil-define-key 'normal 'global (kbd "<leader><tab>b") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader><tab>df") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>l") 'load-file)
(evil-define-key 'normal 'global (kbd "<leader><tab>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader><tab> <tab>") 'switch-to-prev-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab> SPC") 'switch-to-next-buffer)
;; LEADER + X:

;; LEADER + W (Contains bindings related to windows & buffers):
(evil-define-key 'normal 'global (kbd "<leader>w w") 'make-frame)
(evil-define-key 'normal 'global (kbd "<leader>w 1") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>w 2") 'delete-other-windows)
(evil-define-key 'normal 'global (kbd "<leader>w 3") 'split-window-below)
(evil-define-key 'normal 'global (kbd "<leader>w 4") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>w 5") 'split-window-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>w 6") 'split-window-vertically)
