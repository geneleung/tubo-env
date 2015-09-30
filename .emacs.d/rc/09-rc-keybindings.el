;;; 09-rc-keybindings.el --- keybinds
;;; Commentary:
;;; Code:

;; C-F1: 'open-mylist
;; C-S-f1: org-agenda
;; F1: python-describe-symbol for python-mode.
;;     Woman for other modes.

(require 'artist)
(autoload 'smerge-mode "smerge-mode" nil t)

(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun yc/open-eshell ()
  "DOCSTRING"
  (interactive)
  (let ((ebuffer (get-buffer "*eshell*"))
        (dir (expand-file-name default-directory)))
    (if ebuffer
        (progn
          (set-buffer ebuffer)
          (eshell/cd dir)
          (eshell-send-input)
          (switch-to-buffer ebuffer))
      (eshell))))

(lazy-set-key
 (list
  (cons (kbd "<C-x C-j>") 'dired-jump)
  (cons (kbd "<C-x C-r>") 'recentf-open-files)
  (cons "\C-xt"   'untabify)
  (cons "\C-xT"   'tabify)
  (cons "\C-cff"  'find-function)
  (cons "\C-cfc"  'find-function-on-key)
  ;;;; disabled to use helm-mode
  ;; (cons "\C-x\C-b" 'ibuffer)
  ;; (cons "\C-x\M-b>" 'ibuffer-update)
  (cons "\C-x\M-f>" 'fill-paragraph)
  (cons "\C-x\M-l>" 'fill-region)
  ;; (cons "\C-o" 'zl-newline)
  ;; (cons (kbd "C-S-o") 'zl-newline-up)
  (cons (kbd "<C-S-f2>") 'artist-mode) ;; Provided by artist.
  (cons (kbd "<C-S-f4>") 'kmacro-end-and-call-macro)
  (cons (kbd "<C-f10>") 'bookmark-bmenu-list)
  (cons (kbd "<C-f2>") 'rename-buffer)
  (cons (kbd "<C-f4>") 'kmacro-start-macro-or-insert-counter)
  (cons (kbd "<C-f7>") 'attentions-toggle)
  (cons (kbd "<C-f9>") 'next-error)
  (cons (kbd "<S-f4>") ' kmacro-end-or-call-macro)
  (cons (kbd "<S-f5>") 'ansi-term)
  (cons (kbd "<C-f5>") 'eshell)
  (cons (kbd "<f2>") 'auto-rename-buffer)
  (cons (kbd "C-,") 'backward-page)
  (cons (kbd "C-.") 'forward-page)
  (cons (kbd "C->") 'end-of-buffer)
  (cons (kbd "C-<") 'beginning-of-buffer)
  (cons (kbd "C-w") 'kill-region)
  (cons [(meta ?/)] 'hippie-expand)
  (cons (kbd "<M-return>") 'kill-current-buffer)
  (cons [f4] 'goto-line)
  (cons [f5] 'yc/open-eshell)
  (cons [mouse-4] 'down-slightly)
  (cons [mouse-5] 'up-slightly)
  (cons (kbd "<C-return>") 'yc/insert-line-number)
  (cons "\C-xra"  'yc/insert-register)
  ))

;; Remaps some keybindings.
(yc/eval-after-load
 "flyspell"
 (substitute-key-definition
  'flyspell-goto-next-error  'backward-page flyspell-mode-map)
 (substitute-key-definition
  'flyspell-auto-correct-word 'forward-page flyspell-mode-map))

;; Redefine it here to overwrite it
(define-key ctl-x-map "\C-r" 'recentf-open-files)
;; (define-key ctl-x-map "\C-f" 'ido-find-file)


(provide '09-rc-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; 09-rc-keybindings.el ends here
