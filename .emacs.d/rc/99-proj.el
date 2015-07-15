;;; 99-proj.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Scan and load all project files (format: 99x-proj-xxx.el)
;;; Code:

(require 'cl)

;; This is not correct....
(defmacro* def-proj (&key
                     (name nil)
                     (path nil)
                     (file nil)
                     (spp-files nil)
                     (inc-dirs nil)
                     (sys-inc-dirs nil)
                     (spp-defs nil))
  "Define projects."
  (list
   ede-cpp-root-project  `,(or name (file-name-base path))
    :file         `,(concat path "/" file)
    :include-path `,inc-dirs
    :spp-files    `,(mapcar (lambda (x) (concat path x)) spp-files
                            ;; (remove-if (lambda (x)
                            ;;              (not (file-exists-p x)) spp-files))
                            )
    :system-include-path  `,(mapcar (lambda (x) (concat path x)) sys-inc-dirs)
    ;; :spp-table             `,spp-defs
    ))

(dolist (fn (directory-files "~/.emacs.d/rc/" nil "^99[0-9]+.*?\.el"))
  (condition-case msg
      (load-library fn)
    ('error  (message "Skipped file %s -- %s" fn msg))))

(provide '99-proj)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 99-proj.el ends here
