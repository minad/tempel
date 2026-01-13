;;; tempel.el --- Tempo templates/snippets with in-buffer field editing -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 1.9
;; Package-Requires: ((emacs "29.1") (compat "30"))
;; URL: https://github.com/minad/tempel
;; Keywords: abbrev, languages, tools, text

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tempel implements a simple template/snippet system.  The template format
;; is compatible with the template format of the Emacs Tempo library.  Your
;; templates are stored in the `tempel-path' (by default the file
;; "templates" in the `user-emacs-directory').  Bind the commands
;; `tempel-complete', `tempel-expand' or `tempel-insert' to some keys in
;; your user configuration.  You can jump with the keys M-{ and M-} from
;; field to field.  `tempel-complete' and `tempel-expand' work best with
;; the Corfu completion UI, while `tempel-insert' uses `completing-read'
;; under the hood.  You can also use `tempel-complete' and `tempel-expand'
;; as `completion-at-point-functions'.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup tempel nil
  "Tempo templates/snippets with in-buffer field editing."
  :link '(info-link :tag "Info Manual" "(tempel)")
  :link '(url-link :tag "Website" "https://github.com/minad/tempel")
  :link '(emacs-library-link :tag "Library Source" "tempel.el")
  :group 'abbrev
  :group 'tools
  :group 'matching
  :prefix "tempel-")

(defcustom tempel-path (expand-file-name "templates" user-emacs-directory)
  "A file or a list of template files.
The file paths can contain wildcards, e.g.,
\"~/.config/emacs/templates/*/*.eld\", which matches all
`lisp-data-mode' files in the subdirectories of the templates
directory."
  :type '(choice string (repeat string)))

(defcustom tempel-mark
  #(" " 0 1 (display (space :width (1)) face cursor))
  "Field start indicator."
  :type '(choice (const nil) string))

(defcustom tempel-insert-annotation 40
  "Annotation width for `tempel-insert'."
  :type '(choice (const nil) natnum))

(defcustom tempel-complete-annotation 20
  "Annotation width for `tempel-complete'."
  :type '(choice (const nil) natnum))

(defcustom tempel-user-elements nil
  "List of user element handler functions.
The functions take a template element as argument and must return either
nil or a new template element, which is subsequently evaluated."
  :type 'hook)

(defcustom tempel-template-sources
  (list #'tempel-path-templates)
  "List of template sources.
A source can either be a function or a variable symbol.  The functions
must return a list of templates which apply to the buffer or context."
  :type 'hook)

(defcustom tempel-done-on-region t
  "Automatically finish template when entering region field."
  :type 'boolean)

(defcustom tempel-done-on-next t
  "Automatically finish template on `tempel-next' from last field."
  :type 'boolean)

(defcustom tempel-auto-reload t
  "Reload templates when files specified by `tempel-path' change.
If a file is modified, added or removed, reload the templates."
  :type 'boolean)

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

(defvar tempel--path-templates nil
  "Templates loaded from the `tempel-path'.")

(defvar tempel--history nil
  "Completion history used by `tempel-insert'.")

(defvar tempel--inhibit-hooks nil
  "Inhibit tempel modification change hooks from running.")

(defvar tempel--ignore-condition nil
  "Ignore template condition.")

(defvar-local tempel--active nil
  "List of active templates.
Each template state is a pair, where the car is a list of overlays and
the cdr is an alist of variable bindings.  The template state is attached
to each overlay as the property `tempel--field'.  Furthermore overlays
may be named with `tempel--name' or carry an evaluatable Lisp expression
`tempel--form'.")

(defvar-keymap tempel-map
  :doc "Keymap to navigate across template fields."
  "<remap> <beginning-of-buffer>" #'tempel-beginning
  "<remap> <end-of-buffer>" #'tempel-end
  "<remap> <kill-sentence>" #'tempel-kill
  "<remap> <keyboard-escape-quit>" #'tempel-abort
  "<remap> <backward-paragraph>" #'tempel-previous
  "<remap> <forward-paragraph>" #'tempel-next
  ;; Use concrete keys because of org mode
  "M-RET" #'tempel-done
  "M-{" #'tempel-previous
  "M-}" #'tempel-next
  "M-<up>" #'tempel-previous
  "M-<down>" #'tempel-next)

(defun tempel--print-template (template)
  "Print TEMPLATE."
  (cl-loop
   for elt in template until (keywordp elt) concat
   (pcase elt
     ('nil nil)
     ((pred stringp) elt)
     (`(s ,name) (symbol-name name))
     (`(,(or 'p 'P) ,_ ,name . ,noinsert)
      (and (not (car noinsert)) (symbol-name name)))
     ('> " ")
     ('n> "\n ")
     ((or 'n '& '% 'o) "\n")
     (_ #("_" 0 1 (face shadow))))))

(defun tempel--template-plist (template)
  "Get property list from TEMPLATE list."
  (cl-loop for x on template if (keywordp (car x)) return x))

(defun tempel--insert-doc (template)
  "Insert documentation of TEMPLATE."
  (when-let* ((doc (plist-get (tempel--template-plist template) :doc)))
    (unless (eq ?\n (char-before)) (insert "\n"))
    (insert (propertize "\n" 'face '(:overline t :height 0.1 :extend t)))
    (insert doc)))

(defun tempel--annotate (templates width sep name)
  "Annotate template NAME given the list of TEMPLATES.
WIDTH and SEP configure the formatting."
  (when-let* ((name (intern-soft name))
              (elts (cdr (assoc name templates))))
    (let ((ann (truncate-string-to-width
                (string-trim
                 (replace-regexp-in-string
                  "[ \t\n\r]+" " "
                  (or (plist-get (tempel--template-plist elts) :ann)
                      (tempel--print-template elts))))
                width)))
      (add-face-text-property 0 (length ann) 'completions-annotations t ann)
      (concat sep ann))))

(defun tempel--info-buffer (templates fun name)
  "Create info buffer for template NAME.
FUN inserts the info into the buffer.
TEMPLATES is the list of templates."
  (when-let* ((name (intern-soft name))
              (elts (cdr (assoc name templates))))
    (with-current-buffer (get-buffer-create " *tempel-info*")
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall fun elts)))))

(defun tempel--delete-word (word)
  "Delete WORD before point."
  (let ((beg (max (point-min) (- (point) (length word)))))
    (when (save-excursion (search-backward word beg 'noerror))
      (delete-region beg (point)))))

(defun tempel--exit (templates region name status)
  "Exit function for completion for template NAME and STATUS.
TEMPLATES is the list of templates.
REGION are the current region bounds."
  (when-let* (((not (eq status 'exact)))
              (sym (intern-soft name))
              (template (alist-get sym templates)))
    (tempel--delete-word name)
    (tempel--insert template region)))

(defun tempel--range-modified (ov &rest _)
  "Range overlay OV modified."
  (when (and (not tempel--inhibit-hooks) (= (overlay-start ov) (overlay-end ov)))
    (let ((inhibit-modification-hooks nil)
          (tempel--inhibit-hooks t))
      (tempel--disable (overlay-get ov 'tempel--range)))))

(defun tempel--field-modified (ov after beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (unless tempel--inhibit-hooks
    (let ((inhibit-modification-hooks nil)
          (tempel--inhibit-hooks t))
      (cond
       ;; Erase default text before modification when typing over it at the
       ;; beginning or end. Deleting or editing inside preserves the text.
       ((and (not after) (overlay-get ov 'tempel--default) (eq beg end)
             (or (= beg (overlay-start ov)) (= end (overlay-end ov))))
        (delete-region (overlay-start ov) (overlay-end ov)))
       ;; Update field after modification
       (after
        (let ((st (overlay-get ov 'tempel--field)))
          (unless undo-in-progress
            (move-overlay ov (overlay-start ov) (max end (overlay-end ov))))
          (when-let* ((name (overlay-get ov 'tempel--name)))
            (setf (alist-get name (cdr st))
                  (buffer-substring-no-properties
                   (overlay-start ov) (overlay-end ov))))
          (unless undo-in-progress
            (tempel--synchronize-fields st ov)))))
      (tempel--update-mark ov))))

(defun tempel--synchronize-fields (st current)
  "Synchronize fields of ST, except CURRENT overlay."
  (let ((range (caar st)))
    (dolist (ov (cdar st))
      (unless (eq ov current)
        (save-excursion
          (goto-char (overlay-start ov))
          (let (x)
            (setq x (or (and (setq x (overlay-get ov 'tempel--form))
                             (or (eval x (cdr st)) ""))
                        (and (setq x (overlay-get ov 'tempel--name))
                             (alist-get x (cdr st)))))
            (when x
              (tempel--synchronize-replace (overlay-start ov)
                                           (overlay-end ov) ov x)))))
      ;; Move range overlay
      (move-overlay range (overlay-start range)
                    (max (overlay-end range) (overlay-end ov))))))

(defun tempel--synchronize-replace (beg end ov str)
  "Replace region between BEG and END with STR.
If OV is alive, move it."
  (let ((old (buffer-substring-no-properties beg end)))
    (setq ov (and ov (overlay-buffer ov) ov))
    (unless (equal str old)
      (unless (eq buffer-undo-list t)
        (push (list 'apply #'tempel--synchronize-replace
                    beg (+ beg (length str)) ov old)
              buffer-undo-list))
      (let ((buffer-undo-list t))
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
INIT is the optional initial input.
Return the added field."
  (let ((ov (make-overlay (point) (point)))
        (hooks (list #'tempel--field-modified)))
    (push ov (car st))
    (when name
      (overlay-put ov 'tempel--name name)
      (setq init (or init (alist-get name (cdr st))))
      (setf (alist-get name (cdr st)) init))
    (when (and init (not (equal init "")))
      (insert init)
      (move-overlay ov (overlay-start ov) (point)))
    (tempel--update-mark ov)
    (overlay-put ov 'tempel--field st)
    (overlay-put ov 'modification-hooks hooks)
    (overlay-put ov 'insert-in-front-hooks hooks)
    (overlay-put ov 'insert-behind-hooks hooks)
    (overlay-put ov 'face 'tempel-field)
    (when (and init (get-text-property 0 'tempel--default init))
      (overlay-put ov 'face 'tempel-default)
      (overlay-put ov 'tempel--default
                   (if (string-suffix-p ": " init) 'end 'start)))
    (tempel--synchronize-fields st ov)
    ov))

(defun tempel--form (st form)
  "Add new template field evaluating FORM to ST.
Return the added field."
  (let ((beg (point)))
    (condition-case nil
        (insert (or (eval form (cdr st)) ""))
      ;; Ignore errors since some variables may not be defined yet.
      (void-variable nil))
    (let ((ov (make-overlay beg (point) nil t)))
      (overlay-put ov 'face 'tempel-form)
      (overlay-put ov 'tempel--form form)
      (push ov (car st))
      ov)))

(defmacro tempel--protect (&rest body)
  "Protect BODY, catch errors."
  `(with-demoted-errors "Tempel Error: %S"
     ,@body))

(defun tempel--element (st region elt)
  "Add template ELT to ST given the REGION."
  (pcase elt
    ('nil)
    ('n (insert "\n"))
    ;; `indent-according-to-mode' fails sometimes in Org. Ignore errors.
    ('n> (insert "\n") (tempel--protect (indent-according-to-mode)))
    ('> (tempel--protect (indent-according-to-mode)))
    ((pred stringp) (insert elt))
    ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
          (insert "\n")))
    ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (insert "\n")))
    ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (open-line 1)))
    (`(s ,name) (tempel--field st name))
    (`(l . ,lst) (dolist (e lst) (tempel--element st region e)))
    ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
    ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
     (if (not region)
         (when-let* ((ov (apply #'tempel--placeholder st rest))
                     ((not rest))
                     (tempel-done-on-region))
           (overlay-put ov 'tempel--enter #'tempel--done))
       (goto-char (cdr region))
       (when (eq (or (car-safe elt) elt) 'r>)
         (indent-region (car region) (cdr region) nil))))
    ;; TEMPEL EXTENSION: Quit template immediately
    ('q (overlay-put (tempel--field st) 'tempel--enter #'tempel--done))
    (_ (if-let* ((ret (run-hook-wrapped 'tempel-user-elements
                                        (lambda (hook elt fields)
                                          (condition-case nil
                                              (funcall hook elt)
                                            (wrong-number-of-arguments
                                             (funcall hook elt fields))))
                                        elt (cdr st))))
           (tempel--element st region ret)
         ;; TEMPEL EXTENSION: Evaluate forms
         (tempel--form st elt)))))

(defun tempel--placeholder (st &optional prompt name noinsert)
  "Handle placeholder element and add field with NAME to ST.
If NOINSERT is non-nil do not insert a field, only bind the value to NAME.
PROMPT is the optional prompt/default value.
If a field was added, return it."
  (setq prompt
        (cond
         ((and (stringp prompt) noinsert) (read-string prompt))
         ((stringp prompt) (propertize prompt 'tempel--default t))
         ;; TEMPEL EXTENSION: Evaluate prompt
         (t (eval prompt (cdr st)))))
  (if noinsert
      (progn (setf (alist-get name (cdr st)) prompt) nil)
    (tempel--field st name prompt)))

(defun tempel--insert (template region)
  "Insert TEMPLATE given the current REGION."
  (let ((plist (tempel--template-plist template)))
    (eval (plist-get plist :pre) 'lexical)
    ;; TODO do we want to have the ability to reactivate snippets?
    (unless (eq buffer-undo-list t)
      (push '(apply tempel--disable) buffer-undo-list))
    (setf (alist-get 'tempel--active minor-mode-overriding-map-alist) tempel-map)
    (save-excursion
      ;; Split existing overlays, do not expand within existing field.
      (dolist (st tempel--active)
        (dolist (ov (cdar st))
          (when (and (<= (overlay-start ov) (point)) (>= (overlay-end ov) (point)))
            (setf (overlay-end ov) (point)))))
      ;; Activate template
      (let ((st (cons nil nil))
            (ov (point))
            (tempel--inhibit-hooks t))
        (cl-loop for x in template until (keywordp x)
                 do (tempel--element st region x))
        (setq ov (make-overlay ov (point) nil t))
        (push ov (car st))
        (overlay-put ov 'modification-hooks (list #'tempel--range-modified))
        (overlay-put ov 'tempel--range st)
        (overlay-put ov 'tempel--post (plist-get plist :post))
        ;;(overlay-put ov 'face 'region) ;; For debugging
        (push st tempel--active)))
    (cond
     ((cl-loop for ov in (caar tempel--active)
               never (overlay-get ov 'tempel--field))
      (goto-char (overlay-end (caaar tempel--active)))
      (tempel--done)) ;; Finalize right away
     ((cl-loop for ov in (caar tempel--active)
               never (and (overlay-get ov 'tempel--field)
                          (eq (point) (overlay-start ov))))
      (tempel-next 1))))) ;; Jump to first field

(defun tempel--save ()
  "Prompt to save modified files in `tempel-path'."
  (cl-loop
   with all = nil
   for (file . _ts) in (car tempel--path-templates) do
   (when-let* ((buf (get-file-buffer file)))
     (with-current-buffer buf
       (when (and (buffer-modified-p)
                  (pcase (or all (read-answer
                                  (format "Save file %s? " file)
                                  '(("yes" ?y "save the file")
                                    ("no"  ?n "skip the file")
                                    ("all" ?! "save all modified files"))))
                    ("yes" t)
                    ("no" nil)
                    ("all" (setq all "all"))))
         (save-buffer buf))))))

(defun tempel--file-read (file)
  "Load templates from FILE."
  (with-temp-buffer
    (insert "(\n")
    (insert-file-contents file)
    (goto-char (point-max))
    (insert "\n)")
    (goto-char (point-min))
    (tempel--file-prepare (read (current-buffer)))))

(defun tempel--file-prepare (data)
  "Reorganize the template DATA from file.
DATA must be a list (modes plist templates modes plist templates...).
See the README, which provides an example for the file format of the
template.eld file.  The return value is a list of (modes plist . templates)."
  (let (result)
    (while data
      (let (modes plist templates)
        (while (and (car data) (symbolp (car data)) (not (keywordp (car data))))
          (push (pop data) modes))
        (while (keywordp (car data))
          (push (pop data) plist)
          (push (pop data) plist))
        (while (consp (car data))
          (push (pop data) templates))
        (push `(,(nreverse modes) ,(nreverse plist) . ,(nreverse templates)) result)))
    result))

(defun tempel-path-templates ()
  "Return templates defined in `tempel-path'.
Additionally, save any files in `tempel-template-sources' that have been
modified since the last time this function was called.
This is meant to be a source in `tempel-template-sources'."
  (when (or (not tempel--path-templates) tempel-auto-reload)
    (let* ((files
            (cl-loop for path in (ensure-list tempel-path)
                     nconc (file-expand-wildcards path t)))
           (timestamps
            (cl-loop
             for f in files collect
             (cons f (time-convert (file-attribute-modification-time
                                    (file-attributes (file-chase-links f)))
                                   'integer)))))
      (unless (equal (car tempel--path-templates) timestamps)
        (setq tempel--path-templates (cons timestamps
                                           (mapcan #'tempel--file-read files))))))
  (tempel--filter-templates (cdr tempel--path-templates)))

(defun tempel--filter-templates (templates)
  "Filter templates from TEMPLATES relevant to the current buffer.
TEMPLATES must be a list in the form (modes plist . templates)."
  (cl-loop for (modes plist . mode-templates) in templates
           if (tempel--condition-p modes plist)
           append mode-templates))

(defun tempel--condition-p (modes plist)
  "Return non-nil if one of MODES matches and the PLIST condition is satisfied."
  (and
   (cl-loop
    for m in modes thereis
    (or (eq m #'fundamental-mode)
        (derived-mode-p m)
        (when-let* ((remap (alist-get m major-mode-remap-alist)))
          (derived-mode-p remap))))
   (or tempel--ignore-condition
       (not (plist-member plist :when))
       (save-excursion
         (save-restriction
           (save-match-data
             (eval (plist-get plist :when) 'lexical)))))))

(defun tempel--templates ()
  "Return templates for current mode."
  (let (result)
    (run-hook-wrapped
     'tempel-template-sources
     (lambda (fun)
       (cond
        ((functionp fun) (cl-callf append result (funcall fun)))
        ((boundp fun) (cl-callf append result (symbol-value fun)))
        (t (error "Template source is not a function or a variable: %S" fun)))
       nil))
    result))

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

(defun tempel--active-p (_sym buffer)
  "Return non-nil if Tempel is active in BUFFER."
  (buffer-local-value 'tempel--active buffer))

(defun tempel-beginning ()
  "Move to beginning of the template."
  (declare (completion tempel--active-p))
  (interactive)
  (when-let* ((pos (tempel--beginning)))
    (cond
     ((/= pos (point)) (goto-char pos))
     (tempel-done-on-next (tempel-done t)))))

(defun tempel-end ()
  "Move to end of the template."
  (declare (completion tempel--active-p))
  (interactive)
  (when-let* ((pos (tempel--end)))
    (cond
     ((/= pos (point)) (goto-char pos))
     (tempel-done-on-next (tempel-done t)))))

(defun tempel--find-overlay (type)
  "Find overlay of TYPE at point."
  (let ((start most-positive-fixnum) field)
    (dolist (ov (overlays-in (max (point-min) (1- (point)))
                             (min (point-max) (1+ (point)))))
      (when (and (overlay-get ov type) (< (overlay-start ov) start))
        (setq start (overlay-start ov) field ov)))
    field))

(defun tempel-kill ()
  "Kill the field contents."
  (declare (completion tempel--active-p))
  (interactive)
  (if-let* ((ov (tempel--find-overlay 'tempel--field)))
      (kill-region (overlay-start ov) (overlay-end ov))
    (kill-sentence nil)))

(defun tempel-next (&optional n)
  "Move N fields forward.
Quit at the end if `tempel-done-on-next' is non-nil."
  (declare (completion tempel--active-p))
  (interactive "p")
  (setq n (or n 1))
  (cl-loop for i below (abs n) do
           (if-let* ((next (tempel--find n)))
               (goto-char next)
             (when tempel-done-on-next
               (tempel-done t))
             (cl-return)))
  ;; Run the enter action of the field.
  (when-let* ((ov (tempel--find-overlay 'tempel--field))
              (fun (overlay-get ov 'tempel--enter)))
    (funcall fun ov)))

(defun tempel-previous (&optional n)
  "Move N fields backward.
Quit at the beginning if `tempel-done-on-next' is non-nil."
  (declare (completion tempel--active-p))
  (interactive "p")
  (tempel-next (- (or n 1))))

(defun tempel--beginning ()
  "Return beginning of template markers."
  (and tempel--active
       (cl-loop for st in tempel--active minimize (overlay-start (caar st)))))

(defun tempel--end ()
  "Return end of template markers."
  (and tempel--active
       (cl-loop for st in tempel--active maximize (overlay-end (caar st)))))

(defun tempel--disable (&optional st)
  "Disable template ST, or last template."
  (if st
      (cl-callf2 delq st tempel--active)
    (setq st (pop tempel--active)))
  (when st
    (mapc #'delete-overlay (car st))
    (unless tempel--active
      (cl-callf2 assq-delete-all 'tempel--active minor-mode-overriding-map-alist))))

(defun tempel--for (all fun)
  "Call FUN for template at point or all templates if ALL is non-nil."
  (declare (indent 1))
  (if all
      (if tempel--active
          (while tempel--active
            (funcall fun (car tempel--active)))
        (user-error "No active templates"))
    (funcall fun (overlay-get
                  (or (tempel--find-overlay 'tempel--range)
                      (user-error "No active template at point"))
                  'tempel--range))))

(defun tempel-done (&optional all)
  "Finish current template.
If prefix argument ALL is given, finish all templates."
  (declare (completion tempel--active-p))
  (interactive "P")
  (tempel--for all #'tempel--done))

(defun tempel-abort (&optional all)
  "Abort current template.
If prefix argument ALL is given, abort all templates."
  (declare (completion tempel--active-p))
  (interactive "P")
  (tempel--for all
    (lambda (st)
      (let ((beg (overlay-start (caar st)))
            (end (overlay-end (caar st))))
        (tempel--disable)
        (delete-region beg end)))))

(defun tempel--done (&optional st)
  "Finalize template ST, or last template."
  (let ((st (if (overlayp st) (overlay-get st 'tempel--field)
              (or st (car tempel--active))))
        (buf (current-buffer)))
    ;; Ignore errors in post expansion to ensure that templates can be
    ;; terminated gracefully.
    (tempel--protect (eval (overlay-get (caar st) 'tempel--post) (cdr st)))
    (with-current-buffer buf
      (tempel--disable st))))

(defun tempel--prefix-bounds ()
  "Return prefix bounds."
  (bounds-of-thing-at-point 'symbol))

;;;###autoload
(defun tempel-expand (&optional interactive)
  "Expand exactly matching template name at point.
This completion-at-point-function (Capf) returns only the single
exactly matching template name.  As a consequence the completion
UI (e.g. Corfu) does not present the candidates for selection.
If you want to select from a list of templates, use
`tempel-complete' instead.  If INTERACTIVE is nil the function
acts like a Capf, otherwise like an interactive completion
command."
  (interactive (list t))
  (when interactive
    (tempel--save))
  (if-let* ((templates (tempel--templates))
            (bounds (bounds-of-thing-at-point 'symbol))
            (name (buffer-substring-no-properties
                   (car bounds) (cdr bounds)))
            (sym (intern-soft name))
            (template (assq sym templates)))
      (if interactive
          (tempel--exit templates nil name 'finished)
        (setq templates (list template))
        (list (car bounds) (cdr bounds) templates
              :category 'tempel
              :exclusive 'no
              :exit-function (apply-partially #'tempel--exit templates nil)))
    (when interactive (user-error "tempel-expand: No matching templates"))))

;;;###autoload
(defun tempel-complete (&optional interactive)
  "Complete template name at point and expand.
This completion-at-point-function (Capf) returns a list of all
possible template names, which are then displayed in the
completion UI (e.g. Corfu) for selection.  See also
`tempel-expand'.  If INTERACTIVE is nil the function acts like a
Capf, otherwise like an interactive completion command."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'tempel-complete))
            completion-cycle-threshold)
        (tempel--save)
        (unless (completion-at-point)
          (user-error "tempel-complete: No matching templates")))
    ;; Use the marked region for template insertion if triggered manually.
    (let ((region (and (eq this-command #'tempel-complete) (tempel--region))))
      (when-let* ((templates (tempel--templates))
                  (bounds (or (and (not region) (bounds-of-thing-at-point 'symbol))
                              (cons (point) (point)))))
        (list (car bounds) (cdr bounds) templates
              :category 'tempel
              :exclusive 'no
              :company-kind (lambda (_) 'snippet)
              :exit-function (apply-partially #'tempel--exit templates region)
              :company-doc-buffer
              (apply-partially #'tempel--info-buffer templates
                               (lambda (elts)
                                 (insert (tempel--print-template elts))
                                 (tempel--insert-doc elts)
                                 (current-buffer)))
              :company-location
              (apply-partially #'tempel--info-buffer templates
                               (lambda (elts)
                                 (pp (cl-loop for x in elts
                                              until (keywordp x) collect x)
                                     (current-buffer))
                                 (tempel--insert-doc elts)
                                 (list (current-buffer))))
              :annotation-function
              (and tempel-complete-annotation
                   (apply-partially #'tempel--annotate
                                    templates tempel-complete-annotation " ")))))))

;;;###autoload
(defun tempel-insert (template-or-name)
  "Insert TEMPLATE-OR-NAME.
If called interactively, select a template with `completing-read'."
  (interactive (list nil))
  (tempel--insert
   (if (consp template-or-name) template-or-name
     (let ((templates (or (tempel--templates)
                          (error "Tempel: No templates for %s" major-mode))))
       (unless template-or-name
         (setq template-or-name
               (intern-soft
                (completing-read
                 "Template: "
                 (lambda (str pred action)
                   (if (eq action 'metadata)
                       `(metadata
                         (category . tempel)
                         ,@(when tempel-insert-annotation
                             `((annotation-function
                                . ,(apply-partially
                                    #'tempel--annotate templates tempel-insert-annotation
                                    #("  " 1 2 (display (space :align-to (+ left 20)))))))))
                     (complete-with-action action templates str pred)))
                 nil t nil 'tempel--history))))
       (or (and template-or-name (alist-get template-or-name templates))
           (user-error "Template %s not found" template-or-name))))
   (tempel--region)))

;;;###autoload
(defmacro tempel-key (key template-or-name &optional map)
  "Bind KEY to TEMPLATE-OR-NAME in MAP."
  (unless (key-valid-p key)
    (error "Invalid key %s" key))
  `(define-key ,(or map 'global-map) ,(key-parse key)
     ,(if (consp template-or-name)
          `(lambda ()
             (interactive)
             (tempel-insert ',template-or-name))
        (let ((cmd (intern (format "tempel-insert-%s" template-or-name))))
          `(prog1 ',cmd
             (defun ,cmd ()
               ,(format "Insert template %s in the current buffer."
                        template-or-name)
               (interactive)
               (tempel-insert ',template-or-name)))))))

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
    (let ((table (make-abbrev-table))
          (tempel--ignore-condition t))
      (dolist (template (tempel--templates))
        (let* ((name (symbol-name (car template)))
               (hook (make-symbol name)))
          (fset hook (lambda ()
                       (tempel--delete-word name)
                       (tempel--insert (cdr template) nil)
                       t))
          (put hook 'no-self-insert t)
          (define-abbrev table name 'Template hook
            :system t :enable-function
            (lambda () (assq (car template) (tempel--templates))))))
      (setq-local abbrev-minor-mode-table-alist
                  (cons `(tempel-abbrev-mode . ,table)
                        abbrev-minor-mode-table-alist)))))

;;;###autoload
(define-globalized-minor-mode global-tempel-abbrev-mode
  tempel-abbrev-mode tempel--abbrev-on
  :group 'tempel)

(defun tempel--abbrev-on ()
  "Enable abbrev mode locally."
  (unless (or noninteractive (eq (aref (buffer-name) 0) ?\s))
    (tempel-abbrev-mode 1)))

(provide 'tempel)
;;; tempel.el ends here
