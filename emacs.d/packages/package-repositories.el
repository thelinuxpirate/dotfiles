;; EVIL??
(require 'package)
(add-to-list 'package-achrives
	     '("melpa" .  "https://melpa.org/packages/") t) ;; MELPA Repo gets added;
(add-to-list 'package-archives
	     '("elpa" . "https://elpa.gnu.org/packages/") t) ;; ELPA Repo gets added;
(package-initialize)
(package-refresh-contents)
