;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: tb-light-theme.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2013-11-15 by Yang,Ying-chao>
;;;
;;;
;;;


(deftheme tb-light
  "My bright side...")

(custom-theme-set-faces
 'tb-light
 '(default((t ( :background "#f8f4d7" :foreground "black"))))
 '(cursor ((t ( :foreground "#c6e2ff" :background "black"))))
 '(mode-line-buffer-id((t ( :foreground "navy" :bold t))))
 '(mode-line((t ( :foreground "black" :background "#dee7f7"))))
 '(mode-line-inactive((t ( :foreground "gray20" :background "gray90"))))
 '(fringe((t ( :background "#eeeeec"))))
 '(region((t ( :background "#a6cafe"))))
 '(header-line((t ( :background "lemonchiffon1"))))
 '(trailing-whitespace((t ( :background "#f57900"))))
 '(font-lock-constant-face((t ( :foreground "dark cyan"))))
 '(font-lock-doc-face((t ( :foreground "#204a87"))))
 '(font-lock-string-face((t ( :foreground "#ce5c00"))))
 '(font-lock-keyword-face((t ( :foreground "Purple" :bold t))))
 '(font-lock-type-face((t ( :foreground "ForestGreen" :bold t))))
 '(font-lock-variable-name-face ((t ( :foreground "sienna" :bold t))))
 '(font-lock-warning-face ((t ( :foreground "#ff0000" :background "#1248d1" :bold t))))
 '(font-lock-function-name-face((t ( :foreground "Blue" :bold t))))
 '(font-lock-builtin-face((t ( :bold t :foreground "lime green"))))
 '(font-lock-comment-face((t (:italic t :slant oblique :foreground "#cc0000"))))
 '(font-lock-preprocessor-face((t ( :italic nil :foreground "lavender"))))
 '(font-lock-preprocessor-face((t ( :foreground "green2" :bold nil))))
 '(rfcview-title-face ((t (:foreground "darkgreen" :weight bold))))
 )

(provide 'tb-light-theme)
;;;;; tb-light-theme.el ends here
