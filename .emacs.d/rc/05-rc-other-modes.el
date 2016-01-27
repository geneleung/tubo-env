;;; 05-rc-other-modes.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(custom-set-variables
 '(generic-extras-enable-list:
   '(alias-generic-mode apache-conf-generic-mode apache-log-generic-mode bat-generic-mode etc-fstab-generic-mode etc-modules-conf-generic-mode etc-passwd-generic-mode etc-services-generic-mode etc-sudoers-generic-mode fvwm-generic-mode hosts-generic-mode ibis-generic-mode inetd-conf-generic-mode inf-generic-mode ini-generic-mode java-manifest-generic-mode java-properties-generic-mode javascript-generic-mode mailagent-rules-generic-mode mailrc-generic-mode named-boot-generic-mode named-database-generic-mode pkginfo-generic-mode prototype-generic-mode rc-generic-mode resolve-conf-generic-mode samba-generic-mode show-tabs-generic-mode vrml-generic-mode x-resource-generic-mode xmodmap-generic-mode)))

(require 'generic-x)

 ;; ****************************** HTTP Code *****************************
;; Explain the meaning of an HTTP status code. Copy httpcode.el to your
;; load-path and add to your .emacs:

(autoload 'hc "httpcode" "http status code" t)

 ;; ****************************** HTML Mode ******************************
(autoload 'html-mode "sgml-mode" "" t)
(add-to-list 'auto-mode-alist
             '("/itsalltext/" . html-mode))

(yc/eval-after-load
 "sgml-mode"
 (defun yc/html-newline ()
   "New line, add <br> into the end of line."
   (interactive)
   (insert "<br />")
   (newline-and-indent)
   )

 (defun yc/html-newph ()
   "New line, add <br> into the end of line."
   (interactive)
   (newline-and-indent)
   (insert "<p>\n\n</p>")
   (backward-char 4)
   (indent-or-complete)
   (forward-line -1)
   (indent-or-complete)
   )

 (defun yc/html-txt-format (fmt)
   "Format a text string (from start to end )into some format definded as yc/fmt."
   (let ((yc/fmt_len (length fmt)))
     (if (= yc/fmt_len 0)
         (error "Unknown format!"))
     (insert (format "<%s></%s>" fmt fmt))
     (backward-char (+ yc/fmt_len 3))
     (yank)))

 (defun yc/html-txt-bd (start end)
   "<strong></strong>"
   (interactive "rp")
   (kill-region start end)
   (yc/html-txt-format "strong")
   )

 (defun yc/html-txt-pre (start end)
   "<pre></pre>"
   (interactive "rp")
   (kill-region start end)
   (yc/html-txt-format "pre")
   )
 (defun yc/html-txt-tt (start end)
   "<pre></pre>"
   (interactive "rp")
   (kill-region start end)
   (yc/html-txt-format "tt")
   )

 (defun yc/html-txt-pha (start end)
   "<p></p>"
   (interactive "rp")
   (kill-region start end)
   (yc/html-txt-format "p")
   )

 (defun yc/html-txt-col (start end)
   "Add colour."
   (interactive "rp")
   (kill-region start end)
   (insert "   <font color=\"#a40000\">

</font>"
           )
   (forward-line -1)
   (yank)
   )

 (defun yc/small-font (start end)
   "Add colour."
   (interactive "rp")
   (kill-region start end)
   (insert "   <font size=\"1\">

</font>"
           )
   (forward-line -1)
   (yank)
   )
 (defun yc/html-ws ()
   "White Space."
   (interactive)
   (insert "&nbsp; ")
   )

 (defun yc/remove-hrefs ()
   "Funtion to remove hyperlinks quickly"
   (interactive)
   (while (re-search-forward "<a href=.*?\">\\(.*?>\\)</a>" nil t)
     (replace-match (match-string 1) nil nil))
   )

 (defun yc/insert-b64-img (fn)
   "Insert a base64 encoded image"
   (interactive "fImage to insert:\n")
   (let* ((fn_ext_name (file-name-extension fn))
          (fn_base_name (file-name-sans-extension (file-name-nondirectory fn)))
          (b64_cmd (format "base64 %s | tr -d '\n'" fn))
          (b64_content (shell-command-to-string b64_cmd)))
     (insert (format
              "<img title=%s src=\"data:image/%s;base64, %s\" alt=\"%s\"/>"
              fn_base_name fn_ext_name b64_content fn))
     ))

 (add-hook 'html-mode-hook (lambda ()
                             (local-set-key (kbd "<C-return>") 'yc/html-newline)
                             (local-set-key (kbd "<C-M-return>") 'yc/html-newph)
                             (local-set-key (kbd "C-c <SPC>") 'yc/html-ws)
                             (local-set-key (kbd "C-c b") 'yc/html-txt-bd)
                             (local-set-key (kbd "C-c p") 'yc/html-txt-pre)
                             (local-set-key (kbd "C-c P") 'yc/html-txt-pha)
                             (local-set-key (kbd "C-c t") 'yc/html-txt-tt)
                             (local-set-key (kbd "C-c c") 'yc/html-txt-col)
                             (local-set-key (kbd "C-c i") 'yc/insert-b64-img)
                             (local-set-key (kbd "C-c s") 'yc/small-font)
                             (yc/basic-prog-keybinding)
                             (html-autoview-mode nil)
                             (setq html-autoview-mode nil)
                             (remove-hook 'after-save-hook 'browse-url-of-buffer t)
                             (flyspell-mode 1)))
 )



 ;; *************************** Org Mode ********************************
(require 'org-loaddefs) ;; All necessary autoloads.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|/TODO\\)$"
                                . org-mode))
(add-to-list 'file-coding-system-alist
             (cons "\\.\\(org\\|org_archive\\|/TODO\\)$"  'utf-8))

(yc/autoload 'org-version "org")

(yc/eval-after-load "org"
                    (push (purecopy (append '(org) (version-to-list (org-version))))
                          package--builtin-versions)
                    (custom-set-variables
                     '(org-ditaa-jar-path "~/.emacs.d/site-lisp/org_contrib/ditaa.jar")
                     '(org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
                     '(org-enforce-todo-checkbox-dependencies t)
                     '(org-hide-leading-stars t)
                     '(org-log-done (quote time))
                     '(org-special-ctrl-a/e t)
                     '(org-special-ctrl-k t)
                     '(org-startup-folded nil)
                     '(org-export-time-stamp-file nil)
                     '(org-confirm-babel-evaluate nil)
                     '(org-startup-indented t)
                     '(org-directory (convert-standard-filename "~/Work/Orgs"))
                     '(org-default-notes-file (expand-file-name
                                               "~/Work/Orgs/notes.org"))
                     '(org-html-postamble nil)
                     '(org-src-lang-modes '(("ocaml" . tuareg)
                                            ("elisp" . emacs-lisp)
                                            ("ditaa" . artist)
                                            ("asymptote" . asy)
                                            ("dot" . graphviz-dot)
                                            ("sqlite" . sql)
                                            ("calc" . fundamental)
                                            ("C" . c)
                                            ("cpp" . c++)
                                            ("C++" . c++)
                                            ("screen" . shell-script)))
                     ;; WAITING: Assigned to others, and waiting for their report.
                     ;; PENDING: Pending for some reason, maybe scheduled but not started because task dependency.
                     '(org-todo-keywords (quote ((sequence "TODO(t)" "WAITING(w)" "DOING(g)"
                                                           "DONE(d)" "CANCELED(c)" "PENDING(p)" ))))
                     '(org-use-property-inheritance t))
                    (org-indent-mode 1)

;;; Key bingdings
                    (defun yc/show-pomodoro-keywords ()
                      "Pomodoro Keywords, used by pomodoro technique "
                      (interactive)
                      ;; highlight additional keywords
                      (font-lock-add-keywords nil '(("\\<\\(TODO \\)"
                                                     1 font-lock-comment-face t)))
                      (font-lock-add-keywords nil '(("\\<\\(DONE \\):"
                                                     1 font-lock-builtin-face t)))
                      (font-lock-add-keywords nil '(("\\<\\(DOING \\):"
                                                     1 font-lock-function-name-face t)))
                      )

                    (defun yc/org-mode-hooks ()
                      "Functions will run when entering org-mode"
                      (interactive)
                      (org-defkey org-mode-map "\C-cl" 'org-store-link)
                      (org-defkey org-mode-map "\C-ca" 'org-agenda)
                      (org-defkey org-mode-map "\C-cb" 'org-iswitchb)
                      (org-defkey org-mode-map (kbd "<C-tab>") 'indent-or-complete)
                      (org-defkey org-mode-map [(control ?,)]     'backward-page)
                      (yc/show-pomodoro-keywords)
                      (flyspell-mode)
                      )

                    (defun org-summary-todo (n-done n-not-done)
                      "Switch entry to DONE when all subentries are done, to TODO otherwise."
                      (let (org-log-done org-log-states)   ; turn off logging
                        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

                    (advice-add
                     'org-html-paragraph :around
                     (lambda (func &rest args)
                       "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
                       (let ((orig-contents (cadr args))
                             (reg-han "[[:multibyte:]]"))
                         (setf (cadr args) (replace-regexp-in-string
                                             (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                                             "\\1\\2" orig-contents))
                         (apply func args))))
                    (substitute-key-definition
                     'org-cycle-agenda-files  'backward-page org-mode-map)

                    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
                    (add-hook 'org-mode-hook 'yc/org-mode-hooks)
                    (org-babel-do-load-languages
                     'org-babel-load-languages
                     '((ditaa . t)
                       (dot . t)))
                    )

(yc/eval-after-load "org-agenda"
                    (custom-set-variables
                     '(org-agenda-files (quote ("~/Work/Orgs/")))
                     '(org-agenda-include-all-todo t)
                     '(org-agenda-include-diary nil)
                     '(org-agenda-skip-deadline-if-done t)
                     '(org-agenda-skip-scheduled-if-done t)
                     '(org-agenda-dim-blocked-tasks (quote invisible))
                     ))

(yc/eval-after-load "org-capture"
                    (custom-set-variables
                     '(org-capture-templates
                       '(("t" "Todo" entry (file+headline "~/Work/Orgs/gtd.org" "Tasks")
                          "\n* TODO %?\n  %i\n  %a")
                         ("p" "Project" entry (file+headline "~/Work/Orgs/gtd.org" "Project")
                          "
** NOTE %?\n %i\n %a" )
                         ("n" "New" entry (file+datetree "~/Work/Orgs/Inbox.org" "Inbox")
                          "* %?\nEntered on %U\n  %i\n  %a")
                         ("i" "Idea" entry (file+headline "~/Work/Orgs/task.org" "Idea")
                          "* %?\nEntered on %U\n  %i\n  %a")
                         ("j" "Journal" entry (file+datetree "~/Work/Orgs/journal.org")
                          "* %?\nEntered on %U\n  %i\n  %a")
                         ))))

(yc/eval-after-load "ox-html"
                    (setq org-html-head-extra
                          "
<style type=\"text/css\">
div.org-src-container {
    font-size: 85%;
    font-family: monospace;
  }
  pre.src {
    background-color:#2e3436;
    color:#fefffe;
  }
body {
  font-family: Arial, sans-serif, 'Helvetica Neue', Helvetica;
}
code, pre {
    border-radius: 5px;
	border: 1px solid #e5e5e5;
}
*:not(pre)>code{
    color: #c03;
    background-color: #f8f8f8;
    font-size: 85%;
    white-space: normal;/*overcome bootstrap nowrap*/
    padding: 3px 4px 0px 4px;/*overcome bootstrap padding*/
}
p {font-size: 15px}
li {font-size: 15px}
</style>"))


(defun open-mylist ()
  (interactive)
  (find-file "~/Work/Orgs/gtd.org"))
(global-set-key [(control f1)] 'open-mylist)
(global-set-key (kbd "<C-S-f1>") 'org-agenda)
(global-set-key (kbd "<M-S-f10>") 'org-capture)

(yc/autoload 'org-ioslide-export-to-html "ox-ioslide")


 ;; *************************** Wiki Mode ******************************
(autoload 'mediawiki-draft "mediawiki"nil t)
(yc/set-mode 'mediawiki-mode (rx (or ".wiki"
                                     ".wikipedia.org"
                                     "127.0.0.1")))
(add-hook 'mediawiki-mode-hook 'turn-on-flyspell)

 ;; *************************** nxml mode for XML *******************

(autoload 'nxml-mode "nxml-mode" "" t)
(add-to-list
 'auto-mode-alist
 (cons (concat "\\."
               (regexp-opt
                '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "rdf" "plist") t) "\\'")
       'nxml-mode))

(yc/eval-after-load "nxml-mode"
                    (defun my-xhtml-extras ()
                      (make-local-variable 'outline-regexp)
                      (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
                      (make-local-variable 'outline-level)
                      (setq outline-level 'my-xhtml-outline-level)
                      (outline-minor-mode 1)
                      (hs-minor-mode 1))

                    (defun my-xhtml-outline-level ()
                      (save-excursion (re-search-forward html-outline-level))
                      (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
                        (if (eq (length tag) 2)
                            (- (aref tag 1) ?0)
                          0)))


                    (defun my-nxml-forward-element ()
                      (interactive)
                      (let ((nxml-sexp-element-flag))
                        (setq nxml-sexp-element-flag (not (looking-at "<!--")))
                        (unless (looking-at outline-regexp)
                          (condition-case nil
                              (nxml-forward-balanced-item 1)
                            (error nil)))))

                    (defun my-nxml-mode-hook ()
                      (local-set-key "\C-c/" 'nxml-finish-element)
                      (auto-fill-mode)
                      (rng-validate-mode)
                      (hs-minor-mode 1)
                      (yc/basic-prog-keybinding)
                      )

                    (custom-set-variables
                     '(nxml-attribute-indent 4)
                     '(nxml-child-indent 4)
                     '(nxml-outline-child-indent 4)
                     '(nxml-auto-insert-xml-declaration-flag t)
                     '(nxml-bind-meta-tab-to-complete-flag t)
                     '(nxml-slash-auto-complete-flag t))

                    (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

                    (add-to-list
                     'hs-special-modes-alist
                     '(nxml-mode
                       "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                       ""
                       "<!--" ;; won't work on its own; uses syntax table
                       (lambda (arg) (my-nxml-forward-element))
                       nil))
                    )



 ;; **************************** Text Mode ***************************

(defun yc/txt-mode-hook ()
  "My hooks for txt-mode."
  (interactive)
  (define-key text-mode-map "\C-c\C-e" 'yc/txt-to-png)
  (setq require-final-newline nil))

(add-hook 'text-mode-hook 'yc/txt-mode-hook)
(add-hook  'artist-mode-hook 'yc/txt-mode-hook)

;; (when (string= system-type "windows-nt")
;;     (add-hook  'artist-mode-hook (lambda () (new-frame))))



 ;; ************************** ChangeLog *****************************

(add-hook 'change-log-mode-hook
          (lambda()
            (local-set-key (kbd "<C-return>")
                           'add-change-log-entry-other-window)
            ))


 ;; ****************************** Edit Server for Chrome ***************************

(autoload 'edit-server-start "edit-server" nil t)
(yc/eval-after-load
 "edit-server"
 (custom-set-variables
  '(edit-server-new-frame nil)
  '(edit-server-url-major-mode-alist
    (list
     (cons (rx (or (: ".css" eow)
                   "Configure.aspx"
                   (: "/admin/plugins" eow)))
           'css-mode)
     (cons (rx (or (: ".htm" (? "l") eow)
                   (: "/posts/" (+ alnum))
                   (: ".asp" eow)))
           'html-mode)
     (cons (rx "wiki" )
           (yc/func-wrapper 'mediawiki-mode "mediawiki")))
    )))

 ;; ***************************** Mode Alias **************************

(yc/set-mode 'conf-mode
             (rx (or "Doxyfile"
                     (: (+? ascii) "." (or "doxy" "doxygen" "service"  "conf" "config" "rc"
                                           "cnf" "options"))
                     (: "fvwm/" (+? ascii))
                     ".globalrc"
                     ".gitmodules"
                     "conf\.d/")
                 eol))


(autoload 'logviewer-mode "logviewer"   "Major mode for editing Logviewer files"  t)
(yc/set-mode 'logviewer-mode (rx (or (: bow "messages" eow)
                                     (: "." (or "log" "LOG" "Log"))
                                     (: (or "log" "LOG" "Log") ".txt"))))
(defun yc/mw-open ()
  "Load function mediawiki-site, and call it."
  (interactive)
  (load-library "mediawiki")
  (mediawiki-site)
  )


(autoload 'qml-mode "qml-mode")
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

 ;; Compilation-mode

(autoload 'compilation-mode-hook "compile")
;; (yc/eval-after-load "compile" (define-key compilation-mode-map [f9] 'next-error-no-select))

 ;; Htmlize mode
(autoload 'htmlize-buffer "htmlize"
  "Convert BUFFER to HTML, preserving colors and decorations." t)
(autoload 'htmlize-region "htmlize"
  "Convert region to HTML, preserving colors and decorations." t)

(yc/eval-after-load "htmlize" (setq htmlize-output-type 'inline-css))

   ;; ant-mode
(autoload 'ant-mode "ant-mode" "ant mode to edit ant scripts."  t)
(add-hook 'ant-mode-hook
          (lambda ()
            (autopair-on)))

(add-to-list 'auto-mode-alist
             (cons (rx bow "build.xmlf" eol) 'ant-mode))

;; Swig Mode
(autoload 'swig-mode "swig-mode" "swig mode"  t)
(add-to-list 'auto-mode-alist
             (cons (rx (or ".i"
                           ".swig") eol)
                   'swig-mode))


;; Yaml mode
(autoload 'yaml-mode "yaml-mode" "yaml mode"  t)
(add-to-list 'auto-mode-alist '("tps[[:ascii:]]*\\.txt\\|\\.\\(?:\\(?:y\\(?:a?ml\\)\\)\\)$"
                                . yaml-mode))


 ;; gradle-mode

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist
             (cons (rx  (: "." (or "gradle" "groovy")) eol) 'groovy-mode))
(setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
                                     interpreter-mode-alist))

(autoload 'groovy-mode "groovy-mode" "Groovy mode." t)
(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)
             ))

(yc/autoload 'bison-mode)
(yc/set-mode 'bison-mode
             (rx "." (or "yy" "y" "jison")))



(provide '05-rc-other-modes)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 05-rc-other-modes.el ends here
