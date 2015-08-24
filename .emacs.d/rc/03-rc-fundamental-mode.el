;;; 03-rc-fundamental-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Settings for all modes..

;;; Code:


 ;; Helm
(autoload 'helm-mode "helm-mode" ""  t)
(custom-set-variables
 '(helm-autoresize-mode t)
 '(helm-M-x-fuzzy-match t))

(yc/eval-after-load
 "helm-grep"
 (setq-default
  helm-grep-in-recurse t)
 (custom-set-variables
  '(helm-grep-default-recurse-command "grep --color=never -d recurse %e -n%cH -e %p %f")
  '(helm-grep-default-command helm-grep-default-recurse-command)))

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

 (defun edit-project ()
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/rc" "^99[0-9]?.*?\.el"))

 (defun edit-rcs ()
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/rc" "^[0-9]+.*?\.el"))

 (defun edit-template ()
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/templates" (rx (or alnum "_"))))

 (defun edit-site-lisp ()
   (interactive)
   (helm-rc-list-directory "~/.emacs.d/site-lisp" (rx (or alnum "_"))))

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
(helm-load-and-bind 'info-at-point "info" [remap info])

(define-key global-map [remap list-buffers] 'ibuffer)

(defun suedit (file)
  "Run sudo edit FILE."
  (interactive "FSudoedit: ")
  (helm-find-file-as-root file))


(yc/autoload 'helm-projectile-find-file "helm-projectile")
(define-key (current-global-map) (kbd "C-x M-f") 'helm-projectile-find-file)



(require 'icomplete)
(icomplete-mode t)


(custom-set-variables
 '(recentf-auto-cleanup 'never)
 '(recentf-max-menu-items 10)
 '(recentf-max-saved-items 200)
 '(recentf-save-file (yc/make-cache-path "recentf")))

(autoload 'recentf-mode "recentf")
(define-key ctl-x-map "\C-r" 'recentf-open-files)

 ;; session
(autoload 'session-initialize "session")
(add-hook 'after-init-hook 'session-initialize)
(custom-set-variables
 '(session-use-package t nil (session))
 '(session-save-file (yc/make-cache-path "session"))
 )

 ;;; ABBREV-MODE;;;

(setq abbrev-file-name  "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(setq-default abbrev-mode t)


(yc/eval-after-load
 "woman"
 (let ((paths
        (cond ((string= system-type "windows-nt")
               (setq woman-manpath '("d:/gnu/share/man")))
              ((string= system-type "darwin")
               (setq woman-manpath (quote ("/opt/usr/share/man"
                                           "/opt/usr/local/share/man/"))))
              (t
               (setq woman-manpath (quote ("/usr/share/man"
                                           "/usr/local/share/man/")))))))
   (custom-set-variables
    '(woman-use-own-frame nil) ;; 不打开新的 frame
    '(woman-manpath paths))))



(provide '03-rc-fundamental-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-rc-fundamental-mode.el ends here
