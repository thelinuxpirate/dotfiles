(require 'evil)
(evil-set-leader 'normal (kbd "SPC")) ;; Defines the leader key to be the <SPACE> key;
(evil-define-key 'normal 'global (kbd "<leader>s") 'eshell)
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>l") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>k") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader><tab>f") 'load-file)
