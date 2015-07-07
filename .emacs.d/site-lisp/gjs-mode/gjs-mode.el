;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;; File: gjs-mode.el
;;
;; Copyright (C) 2014 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <yangyingchao@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.
;;
;; It does nothing but defines a gjs-mode based on js2-mdoe. The reason why
;; adding it, is to give yasnippet a chance to load gnome related code
;; snippets into gjs mode, and give auto-complete a chance to load gjs related
;; configurations.
;;

(require 'js3-mode)

;;;###autoload
(define-derived-mode gjs-mode js3-mode "Gnome Javascript-IDE"
  (message "OK")
  )

(add-to-list 'auto-mode-alist
             (cons (rx "." "gjs" eow)
                   'gjs-mode))

(provide 'gjs-mode)
;;;;; gjs-mode.el ends here
