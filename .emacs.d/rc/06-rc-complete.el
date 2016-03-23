;;; 06-rc-complete.el -- complete
;;; Commentary:
;;; Code:


 ;; hippie settings

;;; hippie-try-expand settings
(yc/eval-after-load "hippie-exp"
  (setq hippie-expand-try-functions-list
        '(
          yas/hippie-try-expand
          try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-whole-kill)))

 ;; ************** Autoinsert templates *****************
(autoload 'auto-insert "autoinsert" "" t)
(add-hook 'find-file-hook 'auto-insert)

(yc/eval-after-load
 "autoinsert"

 (defun auto-update-header-file ()
   ;; function replaces the string '@@@' by the current file
   ;; name. You could use a similar approach to insert name and date into
   ;; your file.
   (interactive)
   (let* ((cmps (split-string buffer-file-name "/"))
          (l (1- (length cmps)))
          (header-guard
           (subst-char-in-string
            ?. ?_
            (upcase (if (> l 0)
                        (concat (nth (1- l) cmps) "_" (nth l cmps))
                      (nth l cmps))))))
     (save-excursion
       (goto-char (point-min))
       (while (search-forward "@@@" nil t)
         (replace-match header-guard)))))

 (defun insert-today ()
   "Insert today's date into buffer"
   (interactive)
   (insert (format-time-string "%Y-%m-%d" (current-time))))

 (defun auto-update-c-source-file ()
   (save-excursion
     ;; Replace HHHH with file name sans suffix
     (while (search-forward "HHHH" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match
          (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                  ".h") t))))
   (save-excursion
     ;; Replace @@@ with file name
     (while (search-forward "@@@" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match (file-name-nondirectory buffer-file-name)))))
   (save-excursion
     ;; replace DDDD with today's date
     (while (search-forward "DDDD" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match "")
         (insert-today)))))

 (defun auto-replace-file-name ()
   (save-excursion
     ;; Replace @@@ with file name
     (while (search-forward "(>>FILE<<)" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match (file-name-nondirectory buffer-file-name) t)))))

 (defun auto-replace-file-name-no-ext ()
   (save-excursion
     ;; Replace @@@ with file name
     (while (search-forward "(>>FILE_NO_EXT<<)" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)) t)))))

 (defun auto-replace-file-name-no-ext-minus ()
   (save-excursion
     ;; Replace @@@ with file name
     (let* ((fn (file-name-sans-extension
                 (file-name-nondirectory buffer-file-name)))
            (bname (if (string-match
                        (rx bol
                            (repeat 4 digit) "-"   ;; year
                            (repeat 1 2 digit) "-" ;; month
                            (repeat 1 2 digit)     ;; day
                            "-" (group (* anything)) eol)
                        fn)
                       (match-string 1 fn)
                     fn))
            (replacement (yc/string-replace
                          bname "-" " ")))
       (message "%s --- %s --- %s" fn bname replacement)
       (while (search-forward "(>>FILE_NO_EXT_MINUS<<)" nil t)
         (save-restriction
           (narrow-to-region (match-beginning 0) (match-end 0))
           (replace-match replacement t))))))

 (defun auto-replace-date-time ()
   (save-excursion
     (goto-char (point-min))
     (while (search-forward "(>>DATE<<)" nil t)
       (save-restriction
         (narrow-to-region (match-beginning 0) (match-end 0))
         (replace-match "" t)
         (insert-today)))))

 (defun auto-update-defaults ()
   (auto-replace-file-name)
   (auto-replace-file-name-no-ext)
   (auto-replace-date-time))

 (custom-set-variables
  '(auto-insert-directory "~/.emacs.d/templates/auto-insert/")
  '(auto-insert-directory "~/.emacs.d/templates/auto-insert/")
  '(auto-insert 'other)
  '(auto-insert-query nil)
  '(auto-insert-alist
    '(
      ("\\.h$"   . ["header.h" auto-update-header-file])
      ;; ("\\.cpp$" . ["insert.cpp" auto-update-c-source-file])
      ;; ("\\.c$" . ["insert.c" auto-update-c-source-file])
      ("\\.i$" . ["insert.i" auto-replace-file-name-no-ext])
      ("\\.swig$" . ["insert.i" auto-replace-file-name-no-ext])
      ("\\.org$" . ["insert.org" auto-replace-file-name-no-ext-minus])
      ("\\.sh$" . ["insert.sh" auto-update-defaults])
      ("\\.gjs$" . ["insert.gjs" auto-update-defaults])
      ("\\.lisp$" . ["insert.lisp" auto-update-defaults])
      ("\\.el$" . ["insert.el" auto-update-defaults])
      ("\\.dot$" . ["insert.dot" auto-update-defaults])
      ("\\.pl$" . ["insert.pl" auto-update-defaults])
      ("\\.py$" . ["insert.py" auto-update-defaults])
      ("\\.rb$" . ["insert.rb" auto-update-defaults])
      ("\\.tex$" . ["insert.tex" auto-update-defaults])
      ("\\.html$" . ["insert.html" auto-update-defaults])
      ("\\.devhelp2$" . ["insert.devhelp2" auto-update-defaults])
      ("\\.ebuild$" . ["insert.ebuild" auto-update-defaults])
      ("\\.sh$" . ["insert.sh" auto-update-defaults])
      ("Doxyfile$" . ["insert.doxyfile" auto-update-defaults])
      (".*yasnippets/.*" . ["insert.snip" auto-update-defaults])
      ((rx (or "CMakeList.txt" "CMakeLists.txt" ".cmake")) .
       ["insert.cmake" auto-update-defaults])))))

;; ******************** Yasnippet ****************************
(autoload 'yas-global-mode "yasnippet" "yas-global-mode" t)

(defmacro yas-with-comment (str)
  "Insert string STR."
  `(let (surrounded)
     (save-excursion
       (beginning-of-line)
       (if (looking-at (rx ,comment-start))
           (setq surrounded t)))
     (with-temp-buffer
       (if surrounded
           (insert ,str)
         (insert (format "%s%s%s" ,comment-start ,str ,comment-end)))

       (auto-update-defaults)
       (buffer-substring-no-properties (point-min) (point-max)))))

(yc/eval-after-load "yasnippet"
  (custom-set-variables
   '(yas-verbosity 2)
   '(yas-indent-line 'auto)
   '(yas-snippet-dirs '("~/.emacs.d/templates/yasnippets"))
   '(yas-prompt-functions '(yas/dropdown-prompt
                           yas/ido-prompt
                           yas/completing-prompt)))
  (add-to-list 'auto-mode-alist
               (cons (rx (or (: ".emacs.d/templates/yasnippets/"
                                (+ alnum)
                                (+? ascii)
                                "/")
                             (: ".snippet" eow)))
                     'snippet-mode))

  (defvar my-yasnippet-dir "~/.emacs.d/templates/yasnippets"  "nil")
  ;; hook for automatic reloading of changed snippets
  (defun update-yasnippets-on-save ()
    (when (string-match "yas-snippets" buffer-file-name)
      (message "Reloading snippets...")
      (yas-load-directory my-yasnippet-dir)))

  (defvar yc/yas-loaded nil "Flag -- if snippets are loaded or not")

  (advice-add
   'yas-reload-all :before
   (lambda (&rest args)
     (if yc/yas-loaded
         (yas-recompile-all))))

  (defun yc/new-snippet (name)
    "Create snippet for current mode."
    (interactive "sSnippet Name: ")
    (defun snippet-save-hook ()
      "hook to run after snippet saved"
      (interactive)
      (yas-reload-all)
      (remove-hook 'after-save-hook 'snippet-save-hook))

    (let* ((mode-mapping
            (list (cons 'lisp-interaction-mode 'emacs-lisp-mode)
                  (cons 'c-mode 'cc-mode)))
           (mode (or (cdr (assq major-mode  mode-mapping)) major-mode))
           (dirname (expand-file-name
                     (format "~/.emacs.d/templates/yasnippets/%s" (symbol-name mode))))
           (filename (concat dirname "/" name)))
      (unless (file-directory-p dirname) (mkdir dirname t))
      (add-hook 'after-save-hook 'snippet-save-hook)
      (find-file filename)))

  (add-hook 'snippet-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'update-yasnippets-on-save)))

  (defun yc/yas-after-snippet ()
    "Format inserted snippets, if possible."
    (cond
     ((member major-mode '(c-mode c++-mode objc-mode))
      (let (new-line-added)
        (unless (fboundp 'emr-cc-format-region)
          (load "emr-c.el"))
        (save-excursion
          (emr-cc-format-region yas-snippet-beg yas-snippet-end))
        (when (and (not (looking-back (rx (or (: bol (* space))
                                              (: (or ")""}"";"))))))
                   (< (point) yas-snippet-end))
          (newline-and-indent)
          (if (looking-at "\n\n") (kill-line)))))
     (t "")))

  (add-hook 'yas-after-exit-snippet-hook 'yc/yas-after-snippet))

(yas-global-mode 1)
(setq yc/yas-loaded t)

(yc/eval-after-load
 'make-mode
 (add-hook 'makefile-mode-hook
           '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed))))

;; (global-set-key (kbd "<C-tab>") 'yas/expand)

 ;; company mode..
(require 'company)
(global-company-mode)

(yc/autoload 'company-yasnippet)

(yc/eval-after-load
 "company"
 (define-key company-active-map [tab] 'company-complete)
 (define-key company-active-map (kbd "TAB") 'company-complete))

(custom-set-variables
 '(company-backends '((company-files company-dabbrev company-abbrev)))
 '(company-minimum-prefix-length 2)
 '(company-idle-delay 0.2))

(dolist (hk '(git-commit-mode-hook mail-mode-hook mediawiki-mode-hook org-mode-hook))
  (add-hook hk (lambda ()
                 (set (make-local-variable 'company-backends)
                      '((company-files ;; company-ispell
                                       company-dabbrev company-abbrev
                                      :with company-yasnippet))))))

(defmacro yc/add-company-backends-with-yasnippet (&rest backends)
  `(set (make-local-variable 'company-backends)
        (push ',(append backends '(:with company-yasnippet)) company-backends)))

(defmacro yc/add-company-backends (&rest backends)
  `(set (make-local-variable 'company-backends)
        (push ',backends company-backends)))

(add-hook 'prog-mode-hook
          (lambda ()
            (yc/add-company-backends
             company-gtags
             company-keywords company-dabbrev-code company-capf)))

(add-hook 'gud-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-files)))))

(add-hook 'java-mode-hook
          (lambda ()
            (yc/add-company-backends-with-yasnippet company-eclim)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (yc/add-company-backends-with-yasnippet company-nxml)))

(add-hook 'css-mode-hook
          (lambda ()
            (yc/add-company-backends-with-yasnippet company-css)))

(add-hook 'cmake-mode-hook
            (lambda ()
              (yc/add-company-backends-with-yasnippet company-cmake)))

(yc/autoload 'company-jedi)

(add-hook 'python-mode-hook
          (lambda ()
            (yc/add-company-backends-with-yasnippet company-jedi)))

(advice-add
 'company--should-begin :around
 (lambda (func &rest args)
   (if     (and (> (point) (point-min))
                (looking-back (rx (>= 2 (or alnum "_"))) (- (point) 2) nil)
                (looking-at (rx (or eow eol))))
       (apply func args)
     nil)))


 ;; Irony mode
(yc/autoload 'company-irony)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when ede-object
              (aif (ede-system-include-path ede-object)
                  (set (make-variable-buffer-local 'irony-additional-clang-options)
                       (mapcar (lambda (dir)
                                 (format "-I %s" dir)) it))))
            (irony-mode 1)
            (yc/add-company-backends-with-yasnippet company-irony :sorted company-gtags)))

(provide '06-rc-complete)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 06-rc-complete.el ends here
