;;; 01-rc-generic.el -- generic settings.
;; Author: YangYingchao <yangyingchao@gmail.com>
;;; Commentary:
;;; Code:


;;; Start debug on error.
(setq debug-on-error nil)
(yc/setup-display)


;;;; User Info 用户信息
(setq user-full-name "Yang,Ying-chao")
(setq user-mail-address "yangyingchao@gmail.com")

(add-hook 'write-file-hooks 'time-stamp)
(yc/eval-after-load
 "time-stamp"
 (setq time-stamp-format "%04y-%02m-%02d by %U")
 )

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
(setq w32-get-true-file-attributes nil)
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
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

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
(setq warning-suppress-types (quote (nil)))

;; 可以递归的使用 minibuffer
(setq enable-recursive-minibuffers t)

;;;; backup settings 备份设置
(let ((emacs-tmp-dir (expand-file-name "~/.emacs.d/tmpfiles/backups")))
  (setq backup-directory-alist   `((".*" . ,emacs-tmp-dir))
        auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
        auto-save-list-file-prefix  emacs-tmp-dir))
(setq delete-old-versions t) ; Delete Old verion of backup files.
(setq kept-new-versions 2)
(setq kept-old-versions 2)
(setq make-backup-files t)
(setq version-control t)

(setq time-stamp-active nil)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S 93free")

(setq ring-bell-function 'ignore)

;;;; 最大化窗口
(let ((shellpath nil))
  (if (or (string= system-type "windows-nt")
          (string= system-type "ms-dos"))
      (setq shellpath "d:\\gnu\\bin\\sh")
    (setq shellpath "/bin/bash")    )
  (setq shell-file-name shellpath)
  (setq-default explicit-shell-file-name shellpath)
  (setenv "SHELL" shell-file-name)
  )

(when (string= "darwin" system-type)
  (setenv "PATH"
          (concat  "/opt/usr/bin:/opt/bin:/usr/local/bin:/opt/bin:"
                   (getenv "PATH"))))

(setq custom-file "~/.emacs.d/rc/10-emacs-custome.el")

;; Remove vc-hooks, I don't use it.
(setq vc-handled-backends nil)
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))


(yc/eval-after-load
 "custom"
 (setq custom-theme-directory "~/.emacs.d/themes"
       custom-safe-themes t))


 ;; Helm
(autoload 'helm-mode "helm-mode" ""  t)
(custom-set-variables
 '(helm-autoresize-mode t)
 '(helm-M-x-fuzzy-match t))

(yc/eval-after-load
 "helm-grep"
 (setq-default
  helm-grep-in-recurse t
  helm-grep-default-recurse-command ;; skip binary files.
  "grep --color=never -d recurse %e -n%cH -e %p %f"
  helm-grep-default-command helm-grep-default-recurse-command))

(yc/eval-after-load
 "helm-ring"
 (define-or-set helm-source-kill-ring
   (helm-build-sync-source "Kill Ring"
     :init (lambda () (helm-attrset 'last-command last-command))
     :candidates #'helm-kill-ring-candidates
     :filtered-candidate-transformer #'helm-kill-ring-transformer
     :action '(("Yank" . helm-kill-ring-action)
               ("Delete" . (lambda (candidate)
                             (cl-loop for cand in (helm-marked-candidates)
                                      do (setq kill-ring
                                               (delete cand kill-ring))))))
     :persistent-action (lambda (_candidate) (ignore))
     :persistent-help "DoNothing"
     :keymap helm-kill-ring-map
     :migemo t
     :multiline t)
   "Source for browse and insert contents of kill-ring."))


(yc/eval-after-load
 "helm"
 (unless (boundp 'completion-in-region-function)
   (define-key lisp-interaction-mode-map [remap completion-at-point]
     'helm-lisp-completion-at-point)
   (define-key emacs-lisp-mode-map       [remap completion-at-point]
     'helm-lisp-completion-at-point))
 (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

 (defvar helm-rc-misc-map
   (let ((map (make-sparse-keymap)))
     (set-keymap-parent map helm-map)
     (define-key map (kbd "C-s")           'helm-ff-run-grep)
     (define-key map (kbd "C-c ?")         'helm-ff-help)
     (delq nil map))
   "Keymap for `helm-emacs-rcs'.")

 (defun helm-rc-action (cand)
   "description"
   (if (file-directory-p cand)
       (helm-rc-list-directory cand)
     (find-file cand)))

 (defun helm-rc-list-directory (dir &optional pattern)
   "description"
   (let ((s    (helm-build-sync-source "Helm-files"
                 :init nil
                 :candidates (lambda ()
                               (directory-files  dir t (or pattern ".*")))
                 :real-to-display (lambda (x)
                                    (file-name-nondirectory x))
                 :fuzzy-match t
                 :persistent-action 'helm-rc-action
                 :action (helm-make-actions
                          "Open File" 'helm-rc-action
                          "Create New" (lambda (x)
                                         (let ((name (completing-read "File Name:" nil)))
                                           (find-file name))))

                 :keymap helm-rc-misc-map)) )

     (helm :sources '(s)
           :buffer "*helm*"
           :preselect nil)))

 ;; redefine some functions with helm.

 (defadvice edit-project (around helm/edit-project ())
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/rc" "^99[0-9]?.*?\.el"))
 (ad-activate 'edit-project)

 (defadvice edit-rcs (around helm/edit-rcs ())
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/rc" "^[0-9]+.*?\.el"))
 (ad-activate 'edit-rcs)

 (defadvice edit-template (around helm/edit-template ())
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/templates" (rx (or alnum "_"))))
 (ad-activate 'edit-template)

 (define-key helm-map (kbd "<M-return>")  'helm-kill-marked-buffers)
 (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
 (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
 (define-key helm-map (kbd "C-z")  'helm-select-action))

(defmacro helm-load-and-bind (sym file key &optional map)
  "Wrapper of [load-and-bind].
It will load `helm-SYM` from helm-FILE, and bind KEY to loaded SYM."
  `(load-and-bind (intern (concat "helm-" (symbol-name ,sym)))
                  (concat "helm-" ,file) ,key ,map))

(helm-load-and-bind 'M-x "command" (kbd "M-x"))
(helm-load-and-bind 'bookmarks "bookmark" [remap bookmark-bmenu-list])
(helm-load-and-bind 'dabbrev "dabbrev" [remap dabbrev-expand])
(helm-load-and-bind 'eshell-history "eshell" [remap eshell-previous-matching-input])
(helm-load-and-bind 'find-files "files" "\C-f" ctl-x-map)
(helm-load-and-bind 'register "ring" [remap insert-register])
(helm-load-and-bind 'occur "regexp" [remap occur])
(helm-load-and-bind 'man-woman "man" [(f1)])
(helm-load-and-bind 'mini "buffers" [remap switch-to-buffer])
(helm-load-and-bind 'show-kill-ring "ring" [remap yank-pop])
(helm-load-and-bind 'surfraw "net" (kbd "C-c C-s"))
(helm-load-and-bind 'find "files" (kbd "C-x M-f"))

(define-key global-map [remap list-buffers] 'ibuffer)

;; Start server.
(server-start)

(provide '02-rc-generic)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 01-rc-generic.el ends here
