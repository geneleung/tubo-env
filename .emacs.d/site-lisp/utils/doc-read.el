;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: doc-read.el, 10-10-2011

(defvar doc-base-dir "/usr/share/doc" "nil")
(defconst doc-html-file "/tmp/emacs-doc-view.html" "nil")
(defvar doc-list nil "nil")

(defconst doc-reader-head "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">
<html lang=\"en\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title> Document reader </title>
  </head>
  <body bgcolor=\"white\" text=\"black\" link=\"#0000FF\" vlink=\"#840084\"
        alink=\"#0000FF\">
    <ol>" "nil")

(defconst doc-reader-tail "    </ol>
  </body>
</html>
" "nil")

(defconst doc-entry-fmt "      <li> <a href=\"%s\"> %s </a> </li>
" "nil")

(defconst doc-suffix
  '(nil "index" "readme" "manual")
  "nil")

(defconst re-pack-name
  (rx (group (+? ascii)) "-" (1+  digit (? ".")) (? "-r" digit))
  "nil")

;;;###autoload
(defun doc-read-generate ( )
  "Read base directory."
  (interactive)
  (mapc
   (lambda (dir)
     (if (file-directory-p dir)
         (let ((dirn (directory-files dir t "^[hH]tml\\>"))
               (bname (file-name-nondirectory dir))
               (html-dir nil)
               (pack-name nil))
           (if dirn
               (setq html-dir (car dirn))
             (setq html-dir dir))
           ;; Add to doc-list if filenames contain "index", "resadme", or
           ;; "manual", or base-filename consist of exactly package name.
           (if (string-match re-pack-name bname)
               (setcar doc-suffix (match-string 1 bname)))
           (mapc
            (lambda (suffix)
              (if suffix
                  (let ((index-file
                         (concat html-dir "/" suffix ".html")))
                    (if (file-exists-p index-file)
                        (add-to-list 'doc-list
                                     (cons (concat
                                            bname " -- "
                                            (upcase suffix))
                                           index-file)))))
              )
            doc-suffix))))
   (directory-files doc-base-dir t))
  (setcar doc-suffix nil)
  (save-excursion
    (find-file doc-html-file)
    (erase-buffer)
    (insert doc-reader-head)
    (mapc (lambda (cell)
            (insert (format doc-entry-fmt (cdr cell) (car cell)))
            )
          doc-list
          )
    (insert doc-reader-tail)
    (save-buffer)
    (kill-buffer)
    )
  (w3m (format "file://%s" doc-html-file))
  )

(if (string= system-type "windows-nt")
    (setq doc-base-dir "d:/gnu/usr/share/doc/")
    )
(global-set-key (kbd "<S-f9>") 'doc-read-generate)
(provide 'doc-read)
;;;;; doc-read.el ends here
