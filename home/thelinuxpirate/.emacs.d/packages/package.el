(load-file "/home/thelinuxpirate/.emacs.d/packages/package-repositories.el") ;; Loads Repositories for Packages;

;; EVIL Mode:
(unless (package-installed-p 'evil)  
  (package-install 'evil))
