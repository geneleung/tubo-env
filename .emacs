;;; .emacs --- init-file of emacs.
;;; Commentary:
;;; Code:

;; Add customized paths to the front of load-path
(let ((tmp-path load-path))
  (mapc
   (lambda (dir)
     (if (file-exists-p dir)
         (let ((default-directory (concat dir "/")))
           (setq load-path
                 (append
                  (let ((load-path (copy-sequence load-path))) ;; Shadow
                    (append
                     (copy-sequence (normal-top-level-add-to-load-path '(".")))
                     (normal-top-level-add-subdirs-to-load-path)))
                  load-path)))))
   (list "~/.emacs.d/site-lisp/" "~/.emacs.d/rc" "~/.emacs.d/elpa")))

 ;;

(defvar package-init-statistic nil "Package loading statistics.")
(defvar init-timestamp nil "Nil.")

;; Function to collect information of packages.
;; attempt to load a feature/library, failing silently
(defun try-require (feature &optional click)
  "Attempt to load a library or module named FEATURE.
Return true if the library given as argument is successfully loaded.
If not, instead of an error, just add the package to a list of missing packages.
If CLICK is t, calculate time cost."
  (let ((timestamp (current-time))
        (package (if (stringp feature) feature (symbol-name feature))))
    (condition-case err
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (if click
            (add-to-list 'package-init-statistic
                         (list (if (stringp feature) feature (symbol-name feature))
                               (float-time (time-since timestamp))
                               (float-time (time-since init-timestamp)))))
      (message "Checking for library `%s'... Found, cost %.2f seconds"
               feature (float-time (time-since timestamp)))
      (error "Failed to load %s" feature))))

 ;; Load all configuration and packages.
(let ((ts-init (current-time)))
  (require '01-rc-generics)
  (require '02-rc-functions)
  (require '03-rc-fundamental-mode)
  (require '04-rc-prog-mode)
  (require '05-rc-misc)
  (require '05-rc-other-modes)
  (require '06-rc-complete)
  (require '09-rc-keybindings)
  (try-require '10-priv)
  (try-require '10-emacs-custome)
  (require '99-proj)

  ;; Report package statistics.
  (message "\n\nShowing package initialization statistics:\n%s"
           (mapconcat (lambda (x)
                        (format "package %s cost %.2f seconds, accumulated: %.2f seconds"
                                (car x) (cadr x) (caddr x)))
                      (reverse package-init-statistic)
                      "\n"
                      ))

  (message "Finished startup in %.2f seconds"
           (float-time (time-since ts-init))))

;;; .emacs ends here
