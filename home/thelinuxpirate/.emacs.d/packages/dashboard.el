(use-package dashboard
  :ensure t
  :config
  
;; Set the title
(setq dashboard-banner-logo-title "Welcome to the PirateShip")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; "/home/thelinuxpirate/.emacs.d/dashboard-icons/bullet-logo.png"
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set:
(setq dashboard-center-content t)

;; Displays Widgets:
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

;; Wiget Headings + Icons:
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;; Modify Widgets Names Example:
;; (setq dashboard-item-names '(("Recent Files:" . "Recently opened files:")
;;                              ("Agenda for today:" . "Today's agenda:")
;;                              ("Agenda for the coming week:" . "Agenda:"))

;; Custom Widget Example:
;; (defun dashboard-insert-custom (list-size)
;;   (insert "Custom text"))
;; (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
;; (add-to-list 'dashboard-items '(custom) t)

;; NAVIGATOR:
(setq dashboard-set-navigator t)

;; Format: "(icon title help action face prefix suffix)"
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

;; Init Message:
(setq dashboard-init-info "Gentoo GNU/Linux/")

;; Foot Messages:
(setq dashboard-set-footer t) ;; Set to Nil to disable footer messages;

;; (setq dashboard-footer-messages '("Dashboard is pretty cool!"))
;; (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
;;                                                   :height 1.1
;;                                                   :v-adjust -0.05
;;                                                   :face 'font-lock-keyword-face))

;; To disable shortcut "jump" indicators for each section, set to nil
(setq dashboard-show-shortcuts t)

;; Dashboard frames for emacs client:
(setq initial-buffer-choice
	(lambda ()
	  (get-buffer-create "*dashboard*"))))
