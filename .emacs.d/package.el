;; This code makes sure that these packages are installed before you start using the system:
(unless (package-installed-p 'evil)  
  (package-install 'evil))

(unless (package-installed-p 'beacon-mode)
  (package-install 'beacon-mode))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

(unless (package-installed-p 'tree-sitter)
  (package-install 'tree-sitter))

(unless (package-installed-p 'tree-sitter-langs)
  (package-install 'tree-sitter-langs))
