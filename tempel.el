;;; tempel.el --- Tempo text templates with in-buffer field editing -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/tempel

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tempel implements a simple template/snippet system. The template
;; format is compatible with the template format of the Emacs Tempo
;; library. Your templates are stored in the `tempel-file' (by default
;; the file "templates" in the `user-emacs-directory'). Bind the
;; commands `tempel-complete', `tempel-expand' or `tempel-insert' to
;; some keys in your user configuration. You can jump with the keys M-{
;; and M-} from field to field. `tempel-complete' and `tempel-expand'
;; work best with the Corfu completion UI, while `tempel-insert' uses
;; `completing-read' under the hood. You can also use `tempel-complete'
;; and `tempel-expand' as `completion-at-point-functions'.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup tempel nil
  "Tempo text templates with in-buffer field editing."
  :group 'editing
  :prefix "tempel-")

(defcustom tempel-file (expand-file-name "templates" user-emacs-directory)
  "Path to the template file."
  :type 'string)

(defcustom tempel-mark
  #(" " 0 1 (display (space :width (1)) face cursor))
  "Field start indicator."
  :type '(choice (const nil) string))

(defcustom tempel-insert-annotation 40
  "Annotation width for `tempel-insert'."
  :type '(choice (const nil integer)))

(defcustom tempel-complete-annotation 20
  "Annotation width for `tempel-complete'."
  :type '(choice (const nil integer)))

(defface tempel-field
  '((((class color) (min-colors 88) (background light))
     :background "#fdf0ff" :foreground "#541f4f")
    (((class color) (min-colors 88) (background dark))
     :background "#230631" :foreground "#e5cfef")
    (t :inherit highlight))
  "Face used for fields.")

(defface tempel-form
  '((((class color) (min-colors 88) (background light))
     :background "#ecf7ed" :foreground "#004000")
    (((class color) (min-colors 88) (background dark))
     :background "#001904" :foreground "#b8e2b8")
    (t :inherit region))
  "Face used for evaluated forms.")

(defface tempel-default
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :foreground "#0f3360" :slant italic)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :foreground "#a8e5e5" :slant italic)
    (t :inherit highlight :slant italic))
  "Face used for default values.")

(defvar tempel--templates nil
  "Templates loaded from the `tempel-file'.")

(defvar tempel--modified nil
  "Modification time of `tempel-file' at the last load.")

(defvar tempel--history nil
  "Completion history used by `tempel-insert'.")

(defvar-local tempel--active nil
  "List of active templates.
Each template state is a pair, where the car is a list of overlays and
the cdr is an alist of variable bindings. The template state is attached
to each overlay as the property `tempel--state'. Furthermore overlays
may be named with `tempel--name' or carry an evaluatable Lisp expression
`tempel--form'.")

(defvar tempel-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-paragraph] #'tempel-next)
    (define-key map [remap backward-paragraph] #'tempel-previous)
    (define-key map [remap keyboard-quit] #'tempel-abort)
    (define-key map [remap keyboard-escape-quit] #'tempel-abort)
    map)
  "Keymap to navigate across template markers.")

(defun tempel--load (file)
  "Load templates from FILE."
  (with-temp-buffer
    (insert "(\n")
    (insert-file-contents file)
    (goto-char (point-max))
    (insert "\n)")
    (goto-char (point-min))
    (let ((templates (read (current-buffer))) result)
      (while (and templates (symbolp (car templates)))
        (push (cons (car templates) (seq-take-while #'consp (cdr templates))) result)
        (setq templates (seq-drop-while #'consp (cdr templates))))
      result)))

(defun tempel--print-element (elt)
  "Return string representation of template ELT."
  (pcase elt
    ('nil nil)
    ((pred stringp) elt)
    (`(s ,name) (symbol-name name))
    (`(,(or 'p 'P) ,_ ,name . ,noinsert)
     (and (not (car noinsert)) (symbol-name name)))
    ((or 'n 'n> '> '& '% 'o) " ")
    (_ "_")))

(defun tempel--annotate (templates width ellipsis sep name)
  "Annotate template NAME given the list of TEMPLATES.
WIDTH, SEP and ELLIPSIS configure the formatting."
  (when-let* ((name (intern-soft name))
              (elts (cdr (assoc name templates))))
    (concat sep
            (truncate-string-to-width
             (replace-regexp-in-string
              "_+" #("_" 0 1 (face shadow))
              (propertize (replace-regexp-in-string
                           "\\s-+" " "
                           (mapconcat #'tempel--print-element elts ""))
                          'face 'completions-annotations))
             width 0 ?\s ellipsis))))

(defun tempel--delete-word (word)
  "Delete WORD before point."
  (let ((beg (max (point-min) (- (point) (length word)))))
    (when (save-excursion (search-backward word beg 'noerror))
      (delete-region beg (point)))))

(defun tempel--exit (templates region name status)
  "Exit function for completion for template NAME and STATUS.
TEMPLATES is the list of templates.
REGION are the current region bouns"
  (unless (eq status 'exact)
    (when-let* ((sym (intern-soft name))
                (template (alist-get sym templates)))
      (tempel--delete-word name)
      (tempel--insert template region))))

(defun tempel--field-modified (ov after beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (cond
   ;; Erase default before modification if at beginning or end
   ((and (not after) (overlay-get ov 'tempel--default)
         (or (= beg (overlay-start ov)) (= end (overlay-end ov))))
    (delete-region (overlay-start ov) (overlay-end ov)))
   ;; Update field after modification
   (after
    (let ((st (overlay-get ov 'tempel--state)))
      (unless undo-in-progress
        (move-overlay ov (overlay-start ov) (max end (overlay-end ov))))
      (when-let (name (overlay-get ov 'tempel--name))
        (setf (alist-get name (cdr st))
              (buffer-substring-no-properties
               (overlay-start ov) (overlay-end ov))))
      (unless undo-in-progress
        (tempel--synchronize-fields st ov)))))
  (tempel--update-mark ov))

(defun tempel--synchronize-fields (st current)
  "Synchronize fields of ST, except CURRENT overlay."
  (dolist (ov (car st))
    (unless (eq ov current)
      (save-excursion
        (goto-char (overlay-start ov))
        (let (x)
          (setq x (or (and (setq x (overlay-get ov 'tempel--form)) (eval x (cdr st)))
                      (and (setq x (overlay-get ov 'tempel--name)) (alist-get x (cdr st)))))
          (when x (tempel--replace (overlay-start ov) (overlay-end ov) ov x)))))))

(defun tempel--replace (beg end ov str)
  "Replace region beween BEG and END with STR.
If OV is alive, move it."
  (let ((old (buffer-substring-no-properties beg end)))
    (setq ov (and ov (overlay-buffer ov) ov))
    (unless (equal str old)
      (unless (eq buffer-undo-list t)
        (push (list 'apply #'tempel--replace beg (+ beg (length str)) ov old)
              buffer-undo-list))
      (with-silent-modifications
        (save-excursion
          (goto-char beg)
          (delete-char (- end beg))
          (insert str)
          (when ov
            (move-overlay ov beg (point))
            (tempel--update-mark ov)))))))

(defun tempel--update-mark (ov)
  "Update field mark and face of OV."
  (unless (overlay-get ov 'tempel--form)
    (when (overlay-get ov 'tempel--default)
      (overlay-put ov 'tempel--default nil)
      (overlay-put ov 'face 'tempel-field))
    (overlay-put ov 'before-string
                 (and (= (overlay-start ov) (overlay-end ov))
                      tempel-mark))))

(defun tempel--field (st &optional name init)
  "Add template field to ST.
NAME is the optional field name.
INIT is the optional initial input."
  (let ((ov (make-overlay (point) (point))))
    (push ov (car st))
    (when name
      (overlay-put ov 'tempel--name name)
      (setq init (or init (alist-get name (cdr st)) ""))
      (setf (alist-get name (cdr st)) init))
    (when (and init (not (equal init "")))
      (insert init)
      (move-overlay ov (overlay-start ov) (point)))
    (tempel--update-mark ov)
    (overlay-put ov 'tempel--state st)
    (overlay-put ov 'modification-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-in-front-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-behind-hooks (list #'tempel--field-modified))
    (overlay-put ov 'face 'tempel-field)
    (when (and init (get-text-property 0 'tempel--default init))
      (overlay-put ov 'face 'tempel-default)
      (overlay-put ov 'tempel--default
                   (if (string-match-p ": \\'" init) 'end 'start)))
    (tempel--synchronize-fields st ov)))

(defun tempel--form (st form)
  "Add new template field evaluating FORM to ST."
  (let ((beg (point)))
    (condition-case nil
        (insert (eval form (cdr st)))
      ;; Ignore errors since some variables may not be defined yet.
      (void-variable nil))
    (let ((ov (make-overlay beg (point) nil t)))
      (overlay-put ov 'face 'tempel-form)
      (overlay-put ov 'tempel--form form)
      (push ov (car st)))))

(defun tempel--element (st region elt)
  "Add template ELT to ST given the REGION."
  (pcase elt
    ('nil)
    ('n (insert "\n"))
    ('n> (insert "\n") (indent-according-to-mode))
    ('> (indent-according-to-mode))
    ((pred stringp) (insert elt))
    ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
          (insert "\n")))
    ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (insert "\n")))
    ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
	  (open-line 1)))
    (`(s ,name) (tempel--field st name))
    ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
    ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
     (if (not region) (apply #'tempel--placeholder st rest)
       (goto-char (cdr region))
       (when (eq (or (car-safe elt) elt) 'r>)
         (indent-region (car region) (cdr region) nil))))
    (_ (tempel--form st elt)))) ;; TEMPEL EXTENSION: Evaluate forms

(defun tempel--placeholder (st &optional prompt name noinsert)
  "Handle placeholder element and add field with NAME to ST.
If NOINSERT is non-nil do not insert a field, only bind the value to NAME.
PROMPT is the optional prompt/default value."
  (setq prompt
        (cond
         ((and (stringp prompt) noinsert) (read-string prompt))
         ((stringp prompt) (propertize prompt 'tempel--default t))
         ;; TEMPEL EXTENSION: Evaluate prompt
         (t (eval prompt (cdr st)))))
  (if noinsert
      (setf (alist-get name (cdr st)) prompt)
    (tempel--field st name prompt)))

(defun tempel--insert (template region)
  "Insert TEMPLATE given the current REGION."
  ;; TODO do we want to have the ability to reactivate snippets?
  (unless (eq buffer-undo-list t)
    (push (list 'apply #'tempel--disable) buffer-undo-list))
  (setf (alist-get 'tempel--active minor-mode-overriding-map-alist) tempel-map)
  (save-excursion
    ;; Split existing overlays, do not expand within existing field.
    ;; TODO This will be causing issues. Think more about nested expansion.
    (dolist (st tempel--active)
      (dolist (ov (car st))
        (when (and (<= (overlay-start ov) (point)) (>= (overlay-end ov) (point)))
          (setf (overlay-end ov) (point)))))
    ;; Activate template
    (let ((st (cons nil nil))
          (inhibit-modification-hooks t))
      (push (make-overlay (point) (point)) (car st))
      (overlay-put (caar st) 'face 'cursor) ;; TODO debug
      (dolist (elt template) (tempel--element st region elt))
      (push (make-overlay (point) (point) nil t t) (car st))
      (overlay-put (caar st) 'face 'cursor) ;; TODO debug
      (push st tempel--active)))
  ;; Jump to first field
  (unless (cl-loop for ov in (caar tempel--active)
                   thereis (and (overlay-get ov 'tempel--state)
                                (eq (point) (overlay-start ov))))
    (tempel-next 1)))

(defun tempel--save ()
  "Save template file buffer."
  (when-let (buf (get-file-buffer tempel-file))
    (with-current-buffer buf
      (when (and (buffer-modified-p) (y-or-n-p (format "Save file %s? " tempel-file)))
        (save-buffer buf)))))

(defun tempel--templates ()
  "Return templates for current mode."
  (let ((mod (time-convert (file-attribute-modification-time
                            (file-attributes tempel-file))
                           'integer)))
    (unless (equal tempel--modified mod)
      (setq tempel--templates (tempel--load tempel-file)
            tempel--modified mod)))
  (cl-loop for x in tempel--templates
           if (derived-mode-p (car x))
           append (cdr x)))

(defun tempel--region ()
  "Return region bounds."
  (when (use-region-p)
    (when (< (mark) (point)) (exchange-point-and-mark))
    (deactivate-mark)
    (cons (point-marker) (mark-marker))))

(defun tempel--find (dir)
  "Find next overlay in DIR."
  (let ((pt (point)) next stop)
    (dolist (st tempel--active next)
      (dolist (ov (car st))
        (unless (overlay-get ov 'tempel--form)
          (setq stop (if (or (< dir 0) (eq 'start (overlay-get ov 'tempel--default)))
                         (overlay-start ov) (overlay-end ov)))
          (cond
           ((and (> dir 0) (> stop pt))
            (setq next (min (or next (point-max)) stop)))
           ((and (< dir 0) (< stop pt))
            (setq next (max (or next -1) stop)))))))))

(defun tempel-next (arg)
  "Move ARG fields forward and quit at the end."
  (interactive "p")
  (cl-loop for i below (abs arg)
           for next = (tempel--find arg) do
           (if next (goto-char next)
             (tempel-done)
             (cl-return))))

(defun tempel-previous (arg)
  "Move ARG fields backward and quit at the beginning."
  (interactive "p")
  (tempel-next (- arg)))

(defun tempel-abort ()
  "Abort template insertion."
  (interactive)
  ;; TODO abort only the topmost template?
  (when tempel--active
    (let ((beg (cl-loop for st in tempel--active minimize
                        (cl-loop for ov in (car st) minimize (overlay-start ov))))
          (end (cl-loop for st in tempel--active maximize
                        (cl-loop for ov in (car st) maximize (overlay-end ov)))))
      (tempel-done)
      (delete-region beg end))))

(defun tempel--disable ()
  "Disable last template."
  (when-let (st (pop tempel--active))
    (mapc #'delete-overlay (car st))
    (unless tempel--active
      (setq minor-mode-overriding-map-alist
            (delq (assq-delete-all 'tempel--active minor-mode-overriding-map-alist)
                  minor-mode-overriding-map-alist)))))

(defun tempel-done ()
  "Template completion is done."
  (interactive)
  ;; TODO disable only the topmost template?
  (while tempel--active (tempel--disable)))

(defun tempel--interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf))
        completion-cycle-threshold)
    (tempel--save)
    (or (completion-at-point) (user-error "%s: No completions" capf))))

(defun tempel--completion-table (templates)
  "Return a completion table for a list of TEMPLATES.
The completion table specifies the category `tempel'."
  (lambda (str pred action)
    (if (eq action 'metadata)
        '(metadata (category . tempel))
      (complete-with-action action templates str pred))))

;;;###autoload
(defun tempel-expand (&optional interactive)
  "Expand exactly matching template name at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (tempel--interactive #'tempel-expand)
    (when-let* ((templates (tempel--templates))
                (bounds (bounds-of-thing-at-point 'symbol))
                (name (buffer-substring-no-properties
                       (car bounds) (cdr bounds)))
                (sym (intern-soft name))
                (template (assq sym templates)))
      (setq templates (list template))
      (list (car bounds) (cdr bounds)
            (tempel--completion-table templates)
            :exclusive 'no
            :exit-function (apply-partially #'tempel--exit templates nil)))))

;;;###autoload
(defun tempel-complete (&optional interactive)
  "Complete template name at point and expand.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (tempel--interactive #'tempel-complete)
    (when-let (templates (tempel--templates))
      (let* ((region (tempel--region))
             (bounds (or (and (not region) (bounds-of-thing-at-point 'symbol))
                         (cons (point) (point)))))
        (list (car bounds) (cdr bounds)
              (tempel--completion-table templates)
              :exclusive 'no
              :company-kind (lambda (_) 'snippet)
              :exit-function (apply-partially #'tempel--exit templates region)
              :annotation-function
              (and tempel-complete-annotation
                   (apply-partially #'tempel--annotate
                                    templates tempel-complete-annotation nil " ")))))))

;;;###autoload
(defun tempel-insert (name)
  "Insert template by NAME.
If called interactively, select a template with `completing-read'."
  (interactive (list nil))
  (let* ((templates (or (tempel--templates)
                        (error "Tempel: No templates for %s" major-mode)))
         (completion-extra-properties
          (and tempel-insert-annotation
               (list :annotation-function
                     (apply-partially
                      #'tempel--annotate templates tempel-insert-annotation t
                      #("  " 1 2 (display (space :align-to (+ left 20)))))))))
    (unless name
      (setq name (intern-soft (completing-read "Template: " templates
                                               nil t nil 'tempel--history))))
    (tempel--insert (or (alist-get name templates)
                        (user-error "Template %s not found" name))
                    (tempel--region))))

;;;###autoload
(defmacro tempel-key (key name &optional map)
  "Bind KEY to NAME in MAP."
  (let ((cmd (intern (format "tempel-insert-%s" name))))
    `(progn
       (defun ,cmd ()
         ,(format "Insert template %s in the current buffer." name)
         (interactive)
         (tempel-insert ',name))
       (define-key ,(or map 'global-map) ,(kbd key) #',cmd))))

(defun tempel--abbrev-hook (name template)
  "Abbreviation expansion hook for TEMPLATE with NAME."
  (tempel--delete-word name)
  (tempel--insert template nil))

;;;###autoload
(define-minor-mode tempel-abbrev-mode
  "Install Tempel templates as abbrevs."
  :group 'tempel
  :global nil
  (setq-local abbrev-minor-mode-table-alist
              (assq-delete-all 'tempel-abbrev-mode abbrev-minor-mode-table-alist))
  (when (eq abbrev-minor-mode-table-alist
            (default-value 'abbrev-minor-mode-table-alist))
    (kill-local-variable 'abbrev-minor-mode-table-alist))
  (when tempel-abbrev-mode
    (let ((table (make-abbrev-table)))
      (dolist (template (tempel--templates))
        (define-abbrev table (symbol-name (car template)) 'Template
          (apply-partially #'tempel--abbrev-hook (symbol-name (car template)) (cdr template))
          :system t))
      (setq-local abbrev-minor-mode-table-alist
                  (cons `(tempel-abbrev-mode . ,table)
                        abbrev-minor-mode-table-alist)))))

;;;###autoload
(define-globalized-minor-mode tempel-global-abbrev-mode tempel-abbrev-mode
  tempel--abbrev-on :group 'tempel)

(defun tempel--abbrev-on ()
  "Enable abbrev mode locally."
  (unless (or noninteractive (eq (aref (buffer-name) 0) ?\s))
    (tempel-abbrev-mode 1)))

(provide 'tempel)
;;; tempel.el ends here
