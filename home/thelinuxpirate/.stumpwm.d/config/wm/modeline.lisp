(setf *mode-line-timeout* 2)

;; Formating
(setf *time-modeline-string* "%F %H:%M") ;; Date
(load "~/.stumpwm.d/config/wm/visual/colors.lisp")

;; Modeline Colors
(setf *mode-line-background-color* tlp/nord1
      *mode-line-foreground-color* tlp/nord5)
(setf *mode-line-border-color* tlp/nord1
      *mode-line-border-width* 0)

;; Modules
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")

;; Modules Formating
(setf cpu::*cpu-modeline-fmt*        "%c"
      cpu::*cpu-usage-modeline-fmt*  "^f2^f0^[~A~2D%^]"
      mem::*mem-modeline-fmt*        "%a%p"
      *hidden-window-color*          "^**"
      *mode-line-highlight-template* "«~A»")

(("%d") ("^>") ("%C") ("%M") ("%B")) ;; Displayed Contents
