(use-package dashboard
  :ensure t
  :config
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Dotfiles"
         "Browse TheLinuxPirate's Dotfiles"
         (lambda (&rest _) (browse-url "https://github.com/DarthYoshi07/dotfiles")))
        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
        ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          "Rust Handbook"
          "The Rust Book"
          (lambda (&rest _) (browse-url "https://doc.rust-lang.org/book/")))
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

(setq dashboard-set-footer nil) 
(setq dashboard-show-shortcuts t)
(setq initial-buffer-choice
        (lambda ()
          (get-buffer-create "*dashboard*"))))
