;;; tempel-tempo.el --- Tempo Compatibility layer    -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Elias Gabriel Pérez <eg642616@gmail.com>
;; Keywords: abbrev, languages, tools, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ports of some tempo functions and variables.

;;; Code:

(require 'tempel)

;; DONE:


;;; User Options

(defgroup tempel nil
  "Flexible template insertion."
  :prefix "tempo-"
  :group 'tools
  :group 'tempel)

;; NOTE: `tempel-tempo-insert-region' and `tempel-tempo-interactive'
;; must have effect only in `tempo-complete-tag' and
;; `tempo-expand-if-complete'.

(defcustom tempel-tempo-interactive nil
  "Prompt user for strings in templates.
If this variable is non-nil, tempel prompts the user for text to insert
in the templates."
  :type 'boolean)

(defcustom tempel-tempo-insert-region nil
  "Automatically insert current region when there is a `r' in the template.
If this variable is nil, `r' elements will be treated just like `p'
elements, unless the template function is given a prefix (or a non-nil
argument).  If this variable is non-nil, the behavior is reversed."
  :type 'boolean)


;;; Variables

(defvar tempel-tempo-tags nil
  "An association list with tags and corresponding templates.")

(defvar-local tempel-tempo-local-tags nil
  "A list of locally installed tag completion lists.
`tempel-tempo-tags' is always in the last position in this list.")


;;; Functions and Macros

;; Not from tempo.el
(defmacro tempel-tempo--progn (&rest body)
  "Used to provide tempo compatibility to some functions or commands.
This acts like `progn', with BODY."
  `(cl-letf (((symbol-function #'tempel--region)
              (lambda ()
                (when (and (use-region-p)
                           tempel-tempo-insert-region)
                  (when (< (mark) (point)) (exchange-point-and-mark))
                  (cons (point-marker) (mark-marker)))))
             ((symbol-function #'tempel--placeholder)
              (lambda (&optional prompt name noinsert only-prompt)
                (when tempel-tempo-interactive (setq only-prompt t))
                (setq prompt
                      (cond
                       ((and (stringp prompt) (or only-prompt noinsert))
                        (read-string prompt))
                       ((stringp prompt) (propertize prompt 'tempel--default t))
                       ;; TEMPEL EXTENSION: Evaluate prompt
                       (t (eval prompt (cdar tempel--active)))))
                (if noinsert
                    (progn (setf (alist-get name (cdar tempel--active)) prompt) nil)
                  (tempel--field name prompt))
                (deactivate-mark))))
     (let (tempel-done-on-region) ; This var must be disabled
         ,@body)))

;; Not from tempo.el
(defun tempel-tempo--make-templates ()
  "Convert Tempo tags to valid Tempel templates format."
  (mapcar
   (lambda (tags) `(,(car tags) ,@(symbol-value (cdr tags))))
   (append tempel-tempo-local-tags
           tempel-tempo-tags)))

(defun tempel-tempo-save-named (name data)
  "Save NAME with DATA as content for later insertion.
The data can later be retrieved with `tempel-tempo-lookup-named'.

This function returns nil, so it can be used in a template without
inserting anything."
  (setf (alist-get name (cdar tempel--active)) data)
  nil)

(defalias 'tempel-tempo-build-collection 'tempel--templates
  "Build a collection of all the tags and return it.")

;; Currently only used in `tempel-tempo-tests.el'
(defun tempel-tempo-insert-template (template on-region)
  "Insert a template.
TEMPLATE is the template to be inserted.  If ON-REGION is non-nil the
`r' elements are replaced with the current region.  In Transient Mark
mode, ON-REGION is ignored and assumed true if the region is active."
  (let ((tempel-tempo-insert-region
         (or on-region tempel-tempo-insert-region)))
    (tempel-tempo--progn
     (when (symbolp template) (setq template (symbol-value template)))
     (tempel--insert template (tempel--region)))))

(defun tempel-tempo-define-template (name elements &optional tag documentation taglist)
  "Define a template."
  (let ((tag-name (intern (concat "tempel-tempo-template-" name)))
        (taglist (or taglist 'tempel-tempo-tags))
        ;; Add DOCUMENTATION :doc to ELEMENTS unless it already have
        ;; one.
        (elements
         (if (and (not (memq :doc elements))
                  documentation)
             (append elements `(:doc ,documentation))
           elements)))
    (set tag-name elements)
    (fset tag-name (lambda (&optional arg)
                     (:documentation
                      (or documentation (concat "Insert a " name ".")))
                     (interactive "*P")
                     (tempel--insert
                      elements
                      (and (or arg tempel-tempo-insert-region)
                           (tempel--region)))))
    (when tag (tempel-tempo-add-tag tag tag-name taglist))
    tag-name))

(defun tempel-tempo-use-tag-list (tag-list)
  "Install TAG-LIST to be used for template completion in the current buffer.
TAG-LIST is a symbol whose variable value is a tag list created with
`tempo-add-tag'."
  (setq-local tempel-tempo-local-tags (symbol-value tag-list)))


;;; Commands

(defalias 'tempel-tempo-forward-mark 'tempel-next
  "Jump to the next mark.")

(defalias 'tempel-tempo-backward-mark 'tempel-previous
  "Jump to the previous mark.")

(defun tempel-tempo-add-tag (tag template &optional tag-list)
  "Add a template tag.
Add the TAG, that should complete to TEMPLATE to the list in TAG-LIST,
or to `tempel-tempo-tags' if TAG-LIST is nil.  If TAG was already
in the list, replace its template with TEMPLATE."
  (interactive "sTag: \nCTemplate: ")
  (setq tag (intern tag))
  (unless tag-list (setq tag-list 'tempel-tempo-tags))

  (let ((place (assoc tag (symbol-value tag-list))))
    (if place
        ;; Tag is already in the list, assign a new template to it.
        (setcdr place template)
      ;; Tag is not present in the list, add as a new template
      (add-to-list tag-list `(,tag ,@template)))))



(add-hook 'tempel-template-sources #'tempel-tempo--make-templates)

(provide 'tempel-tempo)
;;; tempel-tempo.el ends here
