;; -*-lisp-*-
(defvar tlp/nord0 "#2e3440")
(defvar tlp/nord1 "#3b4252")
(defvar tlp/nord2 "#434c5e")
(defvar tlp/nord3 "#4c566a")
(defvar tlp/nord4 "#d8dee9")
(defvar tlp/nord5 "#e5e9f0")
(defvar tlp/nord6 "#eceff4")
(defvar tlp/nord7 "#8fbcbb")
(defvar tlp/nord8 "#88c0d0")
(defvar tlp/nord9 "#81a1c1")
(defvar tlp/nord10 "#5e81ac")
(defvar tlp/nord11 "#bf616a")
(defvar tlp/nord12 "#d08770")
(defvar tlp/nord13 "#ebcb8b")
(defvar tlp/nord14 "#a3be8c")
(defvar tlp/nord15 "#b48ead")

(setq *colors*
      `(,tlp/nord1   ;; 0 black
        ,tlp/nord11  ;; 1 red
        ,tlp/nord14  ;; 2 green
        ,tlp/nord13  ;; 3 yellow
        ,tlp/nord10  ;; 4 blue
        ,tlp/nord14  ;; 5 magenta
        ,tlp/nord8   ;; 6 cyan
        ,tlp/nord5)) ;; 7 white

(when *initializing*
  (update-color-map (current-screen)))
