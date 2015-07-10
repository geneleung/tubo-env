;;; 04-rc-misc.el -- misc
;;; Commentary:
;;; Code:



(require 'icomplete)
(icomplete-mode t)


;;;; ido -- disabled for helm
;; (require 'ido)
;; (ido-mode 'buffer)
;; (setq ido-enable-flex-matching t) ;; enable fuzzy matching
;; (define-key ctl-x-map "\C-f" 'ido-find-file)


(autoload 'makefile-mode "make-mode" nil t)

(add-to-list 'auto-mode-alist
             (cons (rx (or (: (or "Makefile" "makefile") "." (+ alnum))
                        (: (+ alnum) ".mk"))) 'makefile-mode))


(autoload 'recentf-mode "recentf")
(yc/eval-after-load "recentf"
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 10
        recentf-max-saved-items 200))
(recentf-mode 1)
(define-key ctl-x-map "\C-r" 'recentf-open-files)

;; session
(custom-set-variables
 '(session-use-package t nil (session))
 )

(autoload 'session-initialize "session")
(add-hook 'after-init-hook 'session-initialize)

 ;;; ABBREV-MODE;;;

(setq abbrev-file-name  "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(setq-default abbrev-mode t)

 ;;; Woman
;; (autoload 'woman "woman" "Browse UN*X man page for TOPIC."  t)
;; (defun peer-woman() ;; 这个名字有点罪过……
;;   "Wrap of linux man command"
;;   (interactive)
;;   (let ((woman-topic-at-point t))
;;     (woman)))
;; (global-set-key [(f1)] 'peer-woman)   ;;设定F1为woman快捷键

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

 ;; Info settings.

(setq Info-directory-list nil)
(if (string= system-type "darwin")
    (setq Info-default-directory-list
          (append Info-default-directory-list
                  (list "/opt/usr/share/info"
                        "/opt/usr/share/gcc-data/x86_64-apple-darwin12/4.2.1/info"
                        (format "/opt/usr/share/info/emacs-%s" emacs-major-version)))))

 ;;; ispell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(autoload 'ispell-buffer "ispell" ""  t)
(global-set-key (kbd "<M-S-f11>") 'ispell-buffer)
(autoload 'ispell-word "ispell" ""  t)
(global-set-key (kbd "<S-f11>") 'ispell-word)

(autoload 'flyspell-mode "flyspell" ""  t)
(global-set-key (kbd "<C-f11>") 'flyspell-mode)
(autoload 'flyspell-prog-mode "flyspell" ""  t)
(global-set-key (kbd "<C-S-f11>") 'flyspell-prog-mode)

(yc/eval-after-load "ispell"
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-extra-args '("--reverse"))
  (set-default 'ispell-skip-html t)
  (setq ispell-dictionary "english")
  (setq ispell-local-dictionary "english")
)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

(defun turn-off-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode nil))

(add-hook 'log-edit-mode-hook 'turn-on-flyspell)

(flyspell-prog-mode)

 ;;Dired

;; Ugly, just to ensure "ls" is loaded from "ls-lisp"
(define-or-set ls-lisp-use-insert-directory-program nil)
(load-library "ls-lisp")
(setq ls-lisp-dirs-first t)

(custom-set-variables
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-dirs-first t)
 '(dired-dwim-target t)
 '(dired-dnd-protocol-alist nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top))

(autoload 'dired-jump "dired-x" "" t nil)
(define-key ctl-x-map "\C-j" 'dired-jump)

(yc/eval-after-load
 "dired"
 (require 'dired-async)
 (csq dired-listing-switches "-alh")
  ;use the lisp ls implementation (using this sort by extension works)
  (add-to-list 'auto-mode-alist (cons "[^/]\\.dired$" 'dired-virtual-mode))

  ;; Overwrite some functions
  (defun dired-ediff (file)
    "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for file is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
which is options for `diff'."
    (interactive
     (let* ((current (dired-get-filename t))
            ;; Get the file at the mark.
            (file-at-mark (if (mark t)
                              (save-excursion (goto-char (mark t))
                                              (dired-get-filename t t))))
            ;; Use it as default if it's not the same as the current file,
            ;; and the target dir is the current dir or the mark is active.
            (default (or (if (and (not (equal file-at-mark current))
                                  (or (equal (dired-dwim-target-directory)
                                             (dired-current-directory))
                                      mark-active))
                             file-at-mark)
                         (concat (dired-dwim-target-directory) (file-name-nondirectory current))
                         ))
            (target-dir (if default
                            (dired-current-directory)
                          (dired-dwim-target-directory)))
            (defaults (dired-dwim-target-defaults (list current) target-dir)))
       (list
        (minibuffer-with-setup-hook
            (lambda ()
              (set (make-local-variable 'minibuffer-default-add-function) nil)
              (setq minibuffer-default defaults))
          (read-file-name
           (format "Diff %s with%s: " current
                   (if default (format " (default %s)" default) ""))
           target-dir default t)))))
    (let ((current (dired-get-filename t)))
      (when (or (equal (expand-file-name file)
                       (expand-file-name current))
                (and (file-directory-p file)
                     (equal (expand-file-name current file)
                            (expand-file-name current))))
        (error "Attempt to compare the file to itself"))
      (ediff-files file current)))

  (autoload 'wdired-change-to-wdired-mode "wdired" ""  t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

  ;; Use 7z and tar to compress/decompress file if possible.
  (defvar yc/dired-compress-file-suffixes
    (list
     ;; Regexforsuffix-Programm-Args.
     (list (rx "." (or "tar.gz" "tgz")) "tar" "xzvf")
     (list (rx "." (or "tar.bz2" "tbz")) "tar" "xjvf")
     (list (rx ".tar.xz") "tar" "xJvf")
     (list (rx "." (or "gz" "Z" "z" "dz" "bz2" "xz" "zip" "rar" "7z")) "7z" "x"))
    "nil")

  (defun yc/dired-check-process (msg program &rest arguments)
    (let (err-buffer err (dir default-directory))
      (message "%s..." msg )
      (save-excursion
        ;; Get a clean buffer for error output:
        (setq err-buffer (get-buffer-create " *dired-check-process output*"))
        (set-buffer err-buffer)
        (erase-buffer)
        (setq default-directory dir	; caller's default-directory
              err (not (eq 0 (apply 'process-file program nil t nil
                                    (append (if (string= "7z" program) (list "-y")
                                              nil) arguments)))))
        (if err
            (progn
              (if (listp arguments)
                  (let ((args "") )
                    (mapc (lambda (X)
                            (setq args (concat args X " ")))
                          arguments)
                    (setq arguments args)))
              (dired-log (concat program " " (prin1-to-string arguments) "\n"))
              (dired-log err-buffer)
              (or arguments program t))
          (kill-buffer err-buffer)
          (message "%s...done" msg)
          nil))))

  (defun yc/dired-compress-file (file)
    ;; Compress or uncompress FILE.
    ;; Return the name of the compressed or uncompressed file.
    ;; Return nil if no change in files.
    (let ((handler (find-file-name-handler file 'dired-compress-file))
          suffix newname
          (suffixes yc/dired-compress-file-suffixes))

      ;; See if any suffix rule matches this file name.
      (while suffixes
        (let (case-fold-search)
          (if (string-match (car (car suffixes)) file)
              (setq suffix (car suffixes) suffixes nil))
          (setq suffixes (cdr suffixes))))
      ;; If so, compute desired new name.
      (if suffix
          (setq newname (substring file 0 (match-beginning 0))))
      (cond (handler
             (funcall handler 'dired-compress-file file))
            ((file-symlink-p file)
             nil)
            ((and suffix (nth 1 suffix))
             ;; We found an uncompression rule.
             (if
                 (and (or (not (file-exists-p newname))
                          (y-or-n-p
                           (format "File %s already exists.  Replace it? "
                                   newname)))
                      (not (yc/dired-check-process (concat "Uncompressing " file)
                                                   (nth 1 suffix) (nth 2 suffix) file)))
                 newname))
            (t
           ;;; We don't recognize the file as compressed, so compress it.
           ;;; Try gzip; if we don't have that, use compress.
             (condition-case nil
                 (let ((out-name (concat file ".7z")))
                   (and (or (not (file-exists-p out-name))
                            (y-or-n-p
                             (format "File %s already exists.  Really compress? "
                                     out-name)))
                        (not (yc/dired-check-process (concat "Compressing " file)
                                                     "7z" "a" out-name file))
                        ;; Rename the compressed file to NEWNAME
                        ;; if it hasn't got that name already.
                        (if (and newname (not (equal newname out-name)))
                            (progn
                              (rename-file out-name newname t)
                              newname)
                          out-name))))))))

  (defadvice dired-compress (around yc/dired-compress )
    "customized compress.."
    (let* (buffer-read-only
           (from-file (dired-get-filename))
           (new-file (yc/dired-compress-file from-file)))
      (if new-file
          (let ((start (point)))
            ;; Remove any preexisting entry for the name NEW-FILE.
            (ignore-errors (dired-remove-entry new-file))
            (goto-char start)
            ;; Now replace the current line with an entry for NEW-FILE.
            (dired-update-file-line new-file) nil)
        (dired-log (concat "Failed to compress" from-file))
        from-file))
    )
  (ad-activate 'dired-compress)

    ;; Keybindings for dired-mode
  (lazy-set-key
   (list
    (cons (kbd "M-p") 'dired-up-directory)
    (cons (kbd "C-S-r") 'dired-rar-add-files)
    (cons (kbd "C-j") 'dired-find-file)
    (cons (kbd "<C-return>") 'dired-find-file-other-window)
    (cons (kbd "<C-M-return>") (lambda ()
                                 (interactive)
                                 (yc/open-with-external-app (dired-get-file-for-visit))))
    (cons [f12] 'dired-ediff))
   dired-mode-map))

 ;; image-mode

(yc/eval-after-load
 "image-mode"
 (yc/set-keys (list (cons (kbd "C-c o") 'yc/open-with-external-app))  image-mode-map)
 )


 ;; ediff
(autoload 'diff-mode "diff-mode" ""  t)
(add-to-list 'auto-mode-alist '("\\.rej" . diff-mode))

(autoload 'ediff-files "ediff" ""  t)
(autoload 'ediff-buffers "ediff" "" t)
(autoload 'smerge-ediff "smerge-mode" "Invoke ediff to resolve the conflicts."  t)

(yc/eval-after-load
 "ediff"
 (setq-default ediff-ignore-similar-regions t)
 (setq ediff-diff-ok-lines-regexp
       (concat
        "^\\("
        "[0-9,]+[acd][0-9,]+\C-m?$"
        "\\|[<>] "
        "\\|---"
        "\\|.*Warning *:"
        "\\|.*No +newline"
        "\\|.*missing +newline"
        "\\|.*文件尾没有 newline 字符"
        "\\|^\C-m?$"
        "\\)"))
  (custom-set-variables
   '(ediff-custom-diff-options "-urNa")
   '(ediff-diff-options "")
   '(ediff-split-window-function 'split-window-horizontally)
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   ))

(defun yc/kill-buffer-file (fn)
  "Kill buffer which contains file specified by fn"
  (if (stringp fn)
      (let ((kbuffer (find-file-noselect fn)))
        (when (and (not kbuffer)
                   (string= (buffer-file-name (get-buffer (file-name-nondirectory fn)))
                            fn))
          (setq kbuffer (get-buffer (file-name-nondirectory fn))))
        (if kbuffer
            (kill-buffer kbuffer)))
    (error "Argument should be a string!")))

(defun yc/ediff-files (fn1 fn2)
  "Ediff files with temp-files auto cleaned  up."
  (interactive)
  (message "Files: %s\n%s\n" fn1 fn2)
  (ediff-files fn1
               fn2
               `((lambda ()
                   (make-local-variable 'ediff-cleanup-hook)
                   (setq ediff-cleanup-hook
                         (cons (lambda ()
                                 (if (string-match (rx (+ ascii) "#" (* ascii)) ,fn1)
                                     (yc/kill-buffer-file ,fn1))
                                 (if (string-match (rx (+ ascii) "#" (* ascii)) ,fn2)
                                     (yc/kill-buffer-file ,fn2)))
                               ediff-cleanup-hook))))))
(lazy-set-key
 (list
  ;; (cons "\C-xv+" 'ediff-revision)
  ;; (cons "\C-xv=" 'my-ediff-revision)
  (cons (kbd "<f12>") 'ediff-buffers)
  (cons (kbd "<S-f12>") 'smerge-ediff)
  ))

;; ************************** highlight utils ****************************
(autoload 'highlight-symbol-at-point "highlight-utility"  "" t)
(autoload 'highlight-symbol-next "highlight-utility"  "" t)
(autoload 'highlight-symbol-prev "highlight-utility"  "" t)
(autoload 'highlight-symbol-remove-all "highlight-utility"  "" t)
(autoload 'highlight-symbol-query-replace "highlight-utility"  "" t)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-remove-all)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)


;; ************************** SVN Settings *****************************

(autoload 'svn-checkout "psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
(defalias 'svn-examine 'svn-status)

(autoload 'svn-status "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("svn-commit.*" . log-edit-mode))
(global-set-key "\C-xvs" 'svn-status)
(yc/eval-after-load
 "psvn"
 ;; C-xvv Bind to vc-next-action.
 (global-set-key "\C-xvv" 'svn-status-commit)
 (global-set-key "\C-xvl" 'svn-status-show-svn-log)
 (global-set-key "\C-xvu" 'svn-status-update-cmd)
 (define-key svn-status-mode-map (kbd "d") 'svn-status-rm)
 )



(defun svn-status-curdir()
  (interactive)
  (svn-status (file-name-directory (buffer-file-name))))

 ;; Cool magit tool for git.
(autoload 'magit-status "magit" "magit"  t)
(autoload 'magit-blame-mode "magit-blame" "blame"  t)
(define-or-set magit-last-seen-setup-instructions "1.4.0")
(yc/autoload 'git-commit-training-wheels-mode)

(yc/eval-after-load
 "git-commit-mode"
 (substitute-key-definition
  'kill-buffer  'git-commit-abort git-commit-mode-map)
 (substitute-key-definition
  'ido-kill-buffer  'git-commit-abort git-commit-mode-map))

(global-set-key "\C-xgs" 'magit-status)

(yc/eval-after-load
 "magit"
 (add-hook 'git-commit-mode-hook
           (lambda ()
             ;; (git-commit-training-wheels-mode 1)
             (turn-on-flyspell))) )

(custom-set-variables
 '(magit-revert-buffers t)
 )

(add-hook 'magit-mode-hook
          (lambda ()
            (when (load "magit-svn-mode" t)
              (if (and (file-exists-p ".git/svn/.metadata")
                       (string-match ".*/svn"
                                     (with-temp-buffer
                                       (insert-file-contents ".git/HEAD")
                                       (buffer-substring-no-properties (point-min) (point-max)))))
                  (magit-svn-mode 1)
                (magit-svn-mode -1)))))

 ;; **************************** RFCs ******************************

(autoload 'irfc-visit "irfc" "Open RFC document."  t)
(autoload 'irfc-follow "irfc" "Open RFC document around point."  t)

(yc/eval-after-load
 "irfc"
 (custom-set-variables
  '(irfc-download-base-url "http://www.rfc-editor.org/rfc/")
  '(irfc-directory  "~/Documents/TechBooks/RFCs/")
  ))

(setq auto-mode-alist
      (cons '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . irfc-mode)
            auto-mode-alist))

 ;; ********************* tramp *******************************
(define-or-set tramp-ssh-controlmaster-options
  "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(yc/eval-after-load
 "tramp"
 (setq tramp-default-method (if (member system-type (list 'ms-dos 'windows-nt))
                                (if (executable-find "plink")  "plink" nil)
                              "ssh"))

 ;; (nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods)))
 ;;        '(("bash" "-i")))
 (setq tramp-completion-without-shell-p t)
 ;; (setq tramp-shell-prompt-pattern "^[ $]+")
 (setq tramp-auto-save-directory "~/.emacs.d/auto-save-list")
 (tramp-set-completion-function "ssh"
                                '((tramp-parse-sconfig "/etc/ssh_config")
                                  (tramp-parse-sconfig "~/.ssh/config"))))

 ;; ****************** eww ***************************
(autoload 'eww "eww" "Fetch URL and render the page."  t)
(autoload 'eww-open-file "eww"   "Render a file using EWW."  t)
(autoload 'browse-url-generic "browse-url" ""  t)

(yc/eval-after-load
 "eww"
 (let ((app (cond ((string= system-type "darwin")
                   "/usr/bin/open")
                  ((string= system-type "gnu/linux")
                   (or
                    (executable-find "google-chrome-stable")
                    (executable-find "google-chrome")
                    (executable-find "google-chrome-beta")
                    (executable-find "firefox")
                    (executable-find "firefox-bin")
                    "/usr/bin/xdg-open"))
                  (t nil))))
   (custom-set-variables
    '(browse-url-generic-program app)))

 (defun eww-open-current-page-in-gui ()
   "Opens the current URL in Mozilla Firefox."
   (interactive)
   (browse-url-generic eww-current-url))
 (add-hook 'eww-mode-hook
           (lambda ()
             (setq show-trailing-whitespace nil)))

 (define-key eww-mode-map "\C-co" 'eww-open-current-page-in-gui))



(defun yc/eww-open-this-page ()
  "Call w3m to open this html file"
  (interactive)
  (let ((bn (buffer-file-name)))
    (if (string= (file-name-extension bn) "org")
        (setq fname (concat (file-name-sans-extension bn) ".html"
                            ))
      (setq fname bn))
    (message "Loading %s" fname)
    (eww-open-file fname)))

(global-set-key (kbd "<C-f8>") 'eww)
(global-set-key (kbd "<C-S-f8>") 'yc/eww-open-this-page)

 ;; ***************************** Some extra modes *************************

;; (when (try-require 'doc-read) ;; Depended on w3m.
;;   (message "doc-read loaded."))

 ;; ********************** autocompressings *********************
;; Now add bzip2 support and turn auto compression back on.
(add-to-list 'jka-compr-compression-info-list
             ["\\.dia\\'"
              "compressing" "gzip" ("-c" "-q")
              "uncompressing" "gzip" ("-c" "-q" "-d")
              t t ""]
             )
(jka-compr-update)

(setq mail-default-directory "/tmp/")


(try-require 'sudo-ext)


(autoload 'global-undo-tree-mode "undo-tree" ""  t)
(global-undo-tree-mode)
(define-key undo-tree-map (kbd "\C-x u") 'undo-tree-undo)
(define-key undo-tree-map (kbd "\C-x M-u") 'undo-tree-redo)



(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)


;;; ### Doc-view ###
;;; --- 文档阅读器
(yc/eval-after-load
 "doc-view"
  (csq doc-view-continuous t)
  (lazy-unset-key
   '(".")
   doc-view-mode-map)                     ;卸载按键
  (lazy-set-key
   '(
     ("C-M-j" . doc-view-scroll-up-or-next-page+)       ;翻另一个窗口中图书的下一页
     ("C-M-k" . doc-view-scroll-down-or-previous-page+) ;翻另一个窗口中图书的上一页
     ))
  (lazy-unset-key
   '("x" "M-<" "M->")
   doc-view-mode-map)                     ;卸载一些按键
  (lazy-set-key
   '(
     ([remap scroll-up] . doc-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
     )
   doc-view-mode-map
   )
  (lazy-set-key
   '(
     ("N" . doc-view-next-page)                      ;下一页
     ("P" . doc-view-previous-page)                  ;上一页
     ("." . doc-view-first-page)                     ;第一页
     ("," . doc-view-last-page)                      ;最后一页
     ("g" . doc-view-goto-page)                      ;跳到第几页
     ("e" . doc-view-scroll-down-or-previous-page)   ;向上滚动一屏
     ("SPC" . doc-view-scroll-up-or-next-page)       ;向下滚动一屏
     ("j" . doc-view-next-line-or-next-page)         ;下一行或下一屏
     ("k" . doc-view-previous-line-or-previous-page) ;上一行或上一屏
     ("t" . doc-view-show-tooltip)                   ;当前页提示
     ("q" . bury-buffer)                             ;隐藏buffer
     ("Q" . doc-view-kill-proc-and-buffer)           ;退出并结束进程
     ("C-s" . doc-view-search)                       ;搜索
     ("C-S-n" . doc-view-search-next-match)          ;下一个匹配
     ("C-S-p" . doc-view-search-previous-match)      ;上一个匹配
     ("+" . doc-view-enlarge)                        ;放大页面
     ("-" . doc-view-shrink)                         ;缩小页面
     ("C-c C-c" . doc-view-toggle-display)           ;在文本和图像间切换
     ("C-c C-t" . doc-view-open-text)                ;打开文本
     ("r" . revert-buffer)                           ;刷新
     ("s" . auto-scroll-mode)                        ;自动滚屏
     ("<" . auto-scroll-faster)                      ;加快滚屏速度
     (">" . auto-scroll-slower)                      ;减慢滚屏速度
     )
   doc-view-mode-map
   )
  (setq doc-view-cache-directory "~/.emacs.d/docview"))


(autoload 'stringtemplate-mode "stringtemplate-mode" ""  t)
(add-to-list 'auto-mode-alist '("\\.st\\'" . stringtemplate-mode))


(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(add-to-list 'auto-mode-alist
             '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))

(yc/eval-after-load "desktop-entry-mode" (add-hook 'desktop-entry-mode-hook 'turn-on-font-lock))


(autoload 'eshell-command "eshell" "Execute the Eshell command string COMMAND."  t)
(autoload 'eshell-mode "eshell" "Emacs shell interactive mode."  t)

(defun yc/list-attentions ()
  "List lines that need attentions, such as lines which include XXX or FIXME et.al."
  (interactive)
  (eshell-command
   (concat "egrep -i "
           (rx (? "@") (or "FIXME" "TODO" "DEBUG" "BUG" "YYC:" "XXX" "HACK" "fixme" "todo" "bug")
               (not alnum))  " . -rI"))
  )

(yc/eval-after-load "eshell"
  (require 'esh-io)
  (setq eshell-buffer-shorthand t))


;; (require 'wcy-desktop)
;; (wcy-desktop-init)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (ignore-errors
;;               (wcy-desktop-open-last-opened-files))))

;; (provide 'wcy-desktop-settings)

(autoload 'hexview-find-file "hexview-mode" ""  t)
(lazy-set-key
 (list (cons "\C-xF" 'hexview-find-file)))

 ;; *********************** graphviz dot mode ***********
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "dot mode"  t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

(defalias 'dot-mode 'graphviz-dot-mode)

(yc/eval-after-load
 "graphviz-dot-mode"

 (yc/set-keys
  `((,(kbd "<C-f6>") . 'compile)
    ("\C-co" .  (lambda ()
                (interactive)
                (yc/open-with-external-app
                 (concat (file-name-sans-extension buffer-file-name)
                         "." graphviz-dot-preview-extension))))
    ) graphviz-dot-mode-map)

 (add-hook 'graphviz-dot-mode-hook
           (lambda()
             (setq fill-column 1000)
             (local-set-key (kbd "<C-f6>") 'compile))))

 ;; ********************* hl-line mode ************************
(autoload 'hl-line-mode "hl-line" ""  t)
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (hl-line-mode 1))))
      (list 'dired-mode-hook 'ibuffer-mode-hook 'bookmark-bmenu-mode-hook))


(autoload 'hl-line-mode "hl-line" ""  t)

(mapc (lambda (X)
        (add-hook X
                  (lambda ()
                      (interactive) (hl-line-mode))))
      (list 'bookmark-bmenu-mode-hook 'ibuffer-mode-hook
            'grep-setup-hook))


(autoload 'switch-window "switch-window" ""  t)
(define-key global-map (kbd "C-x o")
  (lambda (count)
    (interactive "p")
    (if (> (length (window-list)) 3)
        (switch-window)
      (other-window 1))))
(define-key global-map (kbd "C-x O")
  (lambda (count)
    (interactive "p")
    (if (> (length (window-list)) 3)
        (switch-window)
      (other-window 2))))

 ;; swiper
(load-and-bind 'swiper "swiper" (kbd "C-S-s"))
(load-and-bind 'ivy-resume "ivy" (kbd "C-S-r"))


 ;; cflow
(autoload 'cflow-mode "cflow-mode" "Major mode for viewing cflow output files"  t)
(defun yc/cflow-function ()
  "Get call graph of inputted function. "
  (interactive)
  (let* ((function (completing-read "C flow for " (list (thing-at-point 'symbol))))
         (cmd (format "cflow  -b --main=%s %s" function buffer-file-name))
         (cflow-buf-name (format "**cflow-%s:%s**"
                                 (file-name-nondirectory buffer-file-name)
                                 function))
         (cflow-buf (get-buffer-create cflow-buf-name)))
    (set-buffer cflow-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (shell-command-to-string cmd))
    (pop-to-buffer cflow-buf)
    (goto-char (point-min))
    (cflow-mode)))

(yc/eval-after-load
 "cc-mode"
 (define-key c-mode-map "\C-ct" 'yc/cflow-function))

 ;; vimrc-mode

(yc/autoload 'vimrc-mode)
(yc/set-mode 'vimrc-mode (rx (or ".vim"
                                (: "." (? "_") (? "g")  "vimrc")
                                ".exrc")))
(add-to-list 'auto-mode-alist
             (cons (rx (or ".vim"
                           (: "." (? "_") (? "g")  "vimrc")
                           ".exrc"))
                   'vimrc-mode))
(provide '05-rc-misc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emacs-rc-misc.el ends here
