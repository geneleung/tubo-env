;;; 99-proj.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Scan and load all project files (format: 99x-proj-xxx.el)
;;; Code:

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
