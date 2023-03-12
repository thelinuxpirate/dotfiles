;; -*-lisp-*-
(set-prefix-key (kbd "s-f"))

(define-key *root-map* (kbd "s-Q") "end-session")
(define-key *root-map* (kbd "RET") "exec alacritty")
(define-key *root-map* (kbd "SPC") "exec dmenu_run")
(define-key *root-map* (kbd "e") "exec emacsclient -c")
