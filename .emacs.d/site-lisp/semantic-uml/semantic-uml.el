;;; semantic-uml.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>
;;
;; Copyright (C) 2015 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <yangyingchao@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Usage:
;; Open a file that can be analyzed by CEDET and select a region, calls uml/struct-to-UML
;; and it will generate a node which can be drawn by graphiviz.
;; (add-hook 'c-mode-common-hook
;; (lambda ()
;;              (local-set-key "\C-csD" 'uml/struct-to-UML)
;;              (local-set-key "\C-csd" 'uml/struct-to-UML-full)))

;;; Code:


 ;; Function used to add fields of struct into a dot file (for Graphviz).

(autoload 'semantic-find-tag-by-overlay "semantic-find")

(require 'cl)
(require 'eieio)

;;;;; Dot templates
(defconst uml/dot-node-head
  "node_%s[shape=record
            label=\"{<f0> %s |\\"
  "Format of node head.")
(defconst uml/dot-node-tail "
}\"];
"
  "Format of node tail.")

(defconst uml/dot-attr "\n%s\\l\\" "Foramt of attr")


;;;;; Regular expressions to match a field of a struct.

(defclass uml/object-node ()
  ((name :initarg :name
         :initform nil
         :type (or null string)
         )
   (funcs :initarg :funcs
          :type (or null list)
          :initform nil
          )
   (attrs :initarg :attrs
          :type (or null list)
          :initform nil
          )
   (subnodes :initarg :subnodes
             :initform nil
             :type (or null list)
             )
   (parents :initarg :parents
            :initform nil
            :type (or null list))
   (mx :initarg :mx
       :type (or null number)
       :initform 0))
  :document "A Semantic object which can be stringified."
  )

(defvar uml/cur-prefix "+"
  "+ : public
   # : protected
   - : private")

(defvar uml/show-func-args nil "Nil.")

(defun uml/stringify-func-args (pl)
  (let (res)
    (dolist (item pl)
      (push (uml/stringify-semantic-ele item t) res))
    (concat "(" (if uml/show-func-args (mapconcat 'identity res ", " ))")")))

(defun uml/parse-tag-element (ele &optional type-only)
  "Parse an element and return a list of string that can be concat into a string."
  (let ((result nil) ;; name added
        (type (semantic-tag-type ele))
        (modifier ""))
    (unless type-only
      (push (semantic-tag-name ele) result))
    (case (semantic-tag-class ele)
      ('function ;; member functions
       (push (uml/stringify-func-args (semantic-tag-get-attribute ele
                                                             :arguments)
                                 ) result))
      ('variable
       (push "" result)))

    (cond
     ((semantic-tag-get-attribute ele :template-specifier)
      (push (format " %s\\<%s\\> " result
                    (uml/stringify-semantic-ele
                     (car (semantic-tag-get-attribute ele
                                                      :template-specifier)))) result))
     ((semantic-tag-get-attribute ele :functionpointer-flag)
      (let ((tmp (car (last result))))
        (setf (car (last result)) (format "(*%s)()" tmp))))
     ((semantic-tag-get-attribute ele :pointer)
      (setq modifier "*"))
     ((semantic-tag-get-attribute ele :dereference)
      (setq modifier "[]")))

    (when type
      (if (semantic-tag-p type)
          (let ((nd (uml/parse-tag-element type)))
            (if (listp nd)
                (setq result (append nd  result) )
              (setq result (push nd  result))))
        (when (stringp type)
          (cond
           ((string= type "class") nil)
           (t (push (concat type modifier) result))))))
    result))

(defun uml/stringify-semantic-ele (ele  &optional type-only)
  "Stringify semantic type"
  (let* ((result (nreverse (uml/parse-tag-element ele type-only)))
         (varname (subseq result 0 2))
         (vartype (subseq result 2))
         (result  (append varname (nreverse vartype))))
    (mapconcat 'identity  result " ")))


(defun uml/stringify-semantic-ele2 (ele)
  "Stringify semantic type"
  (let* ((result (nreverse (uml/parse-tag-element ele)))
         (varname (concat-string-array (subseq result 0 2) " "))
         (vartype (concat-string-array (nreverse (subseq result 2)) " ")))
    (cons (length varname) (cons vartype varname))))

(defun Tag-To-ObjNode (tag &optional fonly)
  "Parse semantic tag and convert it into a uml/object-node which can be stringified later."
  (let* ((type (semantic-tag-get-attribute tag :type))
         (attrs (semantic-tag-get-attribute tag :members))
         (node (make-instance 'uml/object-node))
         (mx 0)
         name func-list attr-list subnodes
         e ele)

    (when (string= type "typedef")
      (let ((tmp-tag (semantic-tag-get-attribute tag :typedef)))
        (setq type (semantic-tag-get-attribute tmp-tag :type)
              attrs (semantic-tag-get-attribute tmp-tag :members))))
    ;;todo: Add more case handling if necessary.

    (setq name (or (semantic-tag-name tag) " "))
    (setq uml/cur-prefix "+")

    (while (setq ele (pop attrs)) ;; Walk through all attributes of this Tag.
      (let ((modifiers (semantic-tag-get-attribute ele :typemodifiers)))
        (when modifiers
          (cond
           ((member "public" modifiers) (setq uml/cur-prefix "+"))
           ((member "protected" modifiers) (setq uml/cur-prefix "#"))
           ((member "private"  modifiers) (setq uml/cur-prefix "-"))))
        (case  (semantic-tag-class ele)
          ('function ;; member functions
           (unless fonly
             (setq e (uml/stringify-semantic-ele2 ele))
             (when e
               (if (> (car e) mx)
                   (setq mx (car e)))
               (add-to-list 'func-list (cons (concat uml/cur-prefix (cddr e)) (cadr e)) t))))
          ('variable ;; memer fields
           (setq e (uml/stringify-semantic-ele2 ele))
           (when e
             (if (> (car e) mx)
                 (setq mx (car e)))
             (add-to-list 'attr-list (cons (concat uml/cur-prefix (cddr e)) (cadr e)) t)))
          ('label    ;; Lables (public/protected/private)
           (setq e (semantic-tag-name ele))
           (cond
            ((string= e "public")
             (setq uml/cur-prefix "+"))
            ((string= e "protected")
             (setq uml/cur-prefix "#"))
            ((string= e "private")
             (setq uml/cur-prefix "-"))
            (t
             (message (format "Skip lable (%s), should be added later."
                              (semantic-tag-name ele))))))
          ('type  ;; Other embedded types, will be processed later.
           (add-to-list 'subnodes (Tag-To-ObjNode (semantic-tag-copy ele) fonly)))
          (t (message (format "Skip type (%s), should be added later."
                              (semantic-tag-class ele)))))))

    (if (not (and  name (or func-list attr-list subnodes)))
        (setq node nil)
      (oset node :name name)
      (oset node :funcs func-list)
      (oset node :attrs attr-list)
      (oset node :subnodes subnodes)
      (oset node :mx mx)
      (oset node :parents (cons (semantic-tag-type-superclasses tag)
                                (semantic-tag-type-interfaces tag)))
      ;; Now process embedded objects.
      )
    node))

(defun uml/allign-concat (e l)
  "Align and concat strings"
  (let* ((result (car e);; (uml/strip-ws-in-string (car e))
                 )
         (cl (length result))
         (padding ""))
    (when (> l cl)
      (dotimes (k (- l cl))
        (setq padding (concat padding "&#32;"))))
    (concat result padding " : " (cdr e))))

(defun uml/connect-parent-node (parents child)
  "Connect this node with its parents"
  (let ((result "")
        (fmt "node_%s -> node_%s [%s];\n")
        (inh "arrowtail=empty style=solid dir=back")
        (imp "arrowtail=empty style=dashed dir=back"))
    (dolist (parent (car parents))
      (setq result (concat result (format fmt parent child inh))))
    (dolist (parent (cdr parents))
      (setq result (concat result (format fmt parent child imp))))
    result))

(defun uml/strip-ws-in-string (src)
  "description"
  (let ((str-array (split-string src split-string-default-separators)))
    (mapconcat 'identity str-array "_")))

(defun uml/stringnify-obj-node (node)
  "description"
  (if (eieio-object-p node)
      (let ((name  (or (oref node :name) "Unamed Object"))
            (funcs (oref node :funcs))
            (attrs (oref node :attrs))
            (subnodes (oref node :subnodes))
            (mx (1+ (oref node :mx)))
            (parents (oref node :parents))
            (node-str "")
            str-list)

        (when name
          ;; start of single node
          (setq node-str (format uml/dot-node-head
                                 (uml/strip-ws-in-string name) name))
          (dolist (attr attrs)
            (setq node-str (concat node-str
                                   (format uml/dot-attr (uml/allign-concat attr mx)))))
          ;; (setq node-str (concat node-str "\n|\\"))
          (dolist (func funcs)
            (setq node-str (concat node-str
                                   (format uml/dot-attr (uml/allign-concat func mx)))))
          (setq node-str (concat node-str uml/dot-node-tail))
          (setq str-list (cons node-str str-list))

          ;; start of subnodes.

          (dolist (subnode subnodes)
            (if subnode
                (setq str-list (append (uml/stringnify-obj-node subnode) str-list))))

          ;; start of node-links..
          (setq str-list (cons (uml/connect-parent-node parents name) str-list)))
        str-list)))

;;;###autoload
(defun uml/struct-to-UML (start end)
  "Generated a UML-like dot graph, with all variables in one line, and all
  functions in one line."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          (strs nil))
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (let ((lst (uml/stringnify-obj-node (Tag-To-ObjNode tag t))))
            (dolist (item lst)
              (add-to-list 'strs item))))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))

;;;###autoload
(defun uml/struct-to-UML-full (start end)
  "Generated a UML-like dot graph, , from region START to END."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          (strs nil))
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (let ((lst (uml/stringnify-obj-node (Tag-To-ObjNode tag))))
            (dolist (item lst)
              (add-to-list 'strs item))))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))


(provide 'semantic-uml)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; semantic-uml.el ends here
