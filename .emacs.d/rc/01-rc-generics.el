;;; 01-rc-generics.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Generic settings irritated to specific mode.

;;; Code:
;;


;;;; User Info
(setq user-full-name "Yang,Ying-chao")
(setq user-mail-address "yangyingchao@gmail.com")

(setq messages-buffer-max-lines t)
(auto-image-file-mode t); 自动加载图像
(column-number-mode t); 显示列数
(fset 'yes-or-no-p 'y-or-n-p); 用y/n替代yes/no
(mouse-avoidance-mode 'animate); 光标碰到鼠标所在位置时，鼠标自动移开
(setq frame-title-format '("" buffer-file-name)); 设置title
(setq inhibit-startup-message t);  关闭启动界面
(setq initial-scratch-message ";;;; This is lisp interactive mode ;;;;\n")
(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode); 默认模式为文本模式
(setq mouse-yank-at-point t); 支持鼠标中键粘贴
(setq visible-bell t); 视觉响铃
(setq x-select-enable-clipboard t)
(setq-default indicate-buffer-boundaries 'left)

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-80col-face)

(setq-default
 mode-line-format
 '(" %z "
   ;; mode-line-buffer-identification
   (:eval
    (cond (buffer-read-only
           (propertize "%b" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "%b" 'face 'mode-line-modified-face))
          (t (propertize "%b" 'face 'mode-line-buffer-id))))
   " "

   ;; is remote or local?
   (:eval (if buffer-file-name mode-line-remote ""))
   (:eval (if buffer-file-name " " ""))

                                        ; Position, including warning for 80 columns
   "%p ("
   (:propertize "%l," 'face 'mode-line)
   (:eval (propertize "%c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line)))
   ") "

                                        ; File size
   (:eval (if buffer-file-name "%I " ""))

   " "
   ;; Major mode
   "%m"

   ;; which function
   " ["
   (:propertize which-func-current
                local-map ,which-func-keymap
                face which-func)
   "] "

   ;; global-mode-string

   " "
   (:eval (format-time-string "%H:%M:%S" (current-time)))))

;; 不要闪烁光标
(blink-cursor-mode 1)

(show-paren-mode t)

;; Some emacs compiled for command line use only have no following settings.
(custom-set-variables
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 )

;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
(setq scroll-margin 3
      scroll-conservatively 10000)

(transient-mark-mode t) ; 高亮显示选中的部分
(setq-default fill-column 78); 列宽, this got reset by (yc/setup-display) or
                                        ; by 100-private.el
(setq-default auto-fill-function 'do-auto-fill)
(setq-default global-font-lock-mode t) ; 语法高亮
(setq sentence-end-double-space nil)
(setq font-lock-maximum-decoration t)
(setq-default show-trailing-whitespace t)
(setq-default max-specpdl-size 8192)
(setq-default max-lisp-eval-depth 8192)
(setq-default large-file-warning-threshold 20000000)

;;;; Garbage Collection
(setq-default garbage-collection-messages nil)
(setq-default gc-cons-threshold 99999999)

;;;; Mode line settings.
(display-time-mode 1)
(custom-set-variables
 '(display-time-24hr-format t)
 '(display-time-day-and-date t))

;; Tab设置
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default show-paren-mode t)
(setq indent-line-function 'indent-relative-maybe)

(put 'upcase-region 'disabled nil) ;;  Enable upcase-region
(put 'downcase-region 'disabled nil);; Enable downcase-region
(put 'set-goal-column 'disabled nil)


;;;; 语言设置
;;(set-language-environment 'Chinese-GBK)
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq system-time-locale "C")

(setq kill-do-not-save-duplicates t)

;; 先格式化再补全
(setq tab-always-indent 'complete)

(setq font-lock-maximum-decoration t)

;; 可以递归的使用 minibuffer
(setq enable-recursive-minibuffers t)

(defvar yc/emacs-cache-dir (expand-file-name "~/.cache/emacs/")
  "Caching directory.")

(defun yc/make-cache-path (path)
  "Compose cache directory for PATH."
  (expand-file-name path yc/emacs-cache-dir ))

;;;; backup settings 备份设置
(let ((emacs-backup-dir (yc/make-cache-path "backups")))
  (setq backup-directory-alist   `((".*" . ,emacs-backup-dir))
        ;; auto-save-file-name-transforms `((".*" ,emacs-backup-dir t))
        auto-save-list-file-prefix  emacs-backup-dir))
(setq delete-old-versions t) ; Delete Old verion of backup files.
(setq kept-new-versions 2)
(setq kept-old-versions 2)
(setq make-backup-files t)
(setq version-control t)

(custom-set-variables
 '(time-stamp-active nil)
 '(time-stamp-warn-inactive t)
 '(time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S 93free"))

(setq ring-bell-function 'ignore)

;; shell path
(let ((shellpath nil))
  (if (or (string= system-type "windows-nt")
          (string= system-type "ms-dos"))
      (setq shellpath "d:\\gnu\\bin\\sh")
    (setq shellpath "/bin/bash")    )
  (setq shell-file-name shellpath)
  (setq-default explicit-shell-file-name shellpath)
  (setenv "SHELL" shell-file-name))

(when (string= "darwin" system-type)
  (setenv "PATH"
          (concat  "/opt/usr/bin:/opt/bin:/usr/local/bin:/opt/bin:"
                   (getenv "PATH"))))

(setq custom-file "~/.emacs.d/rc/10-emacs-custome.el")

;; Remove vc-hooks, I don't use it.
(setq vc-handled-backends nil)
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

(eval-after-load
 "custom"
 (setq custom-theme-directory "~/.emacs.d/themes"
       custom-safe-themes t))

(server-start)
(provide '01-rc-generics)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 01-rc-generics.el ends here
