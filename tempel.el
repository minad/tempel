;;; tempel.el --- Simple templates for Emacs -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/tempel

;; This file is not part of GNU Emacs.

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
;; commands `tempel-expand' or `tempel-insert' to some keys in your user
;; configuration. You can jump with the keys M-{ and M-} from field to
;; field. `tempel-expands' works best with the Corfu completion UI,
;; while `tempel-insert' uses `completing-read' under the hood.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup tempel nil
  "Simple templates"
  :group 'editing
  :prefix "tempel-")

(defcustom tempel-template-locations nil
  "A file or a list of files and/or directories to look for snippets in."
  :type '(choice (string list)))

(defvaralias 'tempel-file 'tempel-template-locations)

(defcustom tempel-mark
  #(" " 0 1 (display (space :width (1)) face cursor))
  "Field start indicator."
  :type '(choice (const nil) string))

(defcustom tempel-insert-annotation 40
  "Annotation width for `tempel-insert'."
  :type '(choice (const nil integer)))

(defcustom tempel-expand-annotation 20
  "Annotation width for `tempel-expand'."
  :type '(choice (const nil integer)))

(defface tempel-field
  '((((class color) (min-colors 88) (background light))
     :background "#fdf0ff" :foreground "#541f4f" :underline "#d5baff")
    (((class color) (min-colors 88) (background dark))
     :background "#230631" :foreground "#e5cfef" :underline "#7042a2")
    (t :inherit highlight))
  "Face used for fields.")

(defface tempel-form
  '((((class color) (min-colors 88) (background light))
     :background "#ecf7ed" :foreground "#004000" :underline "#5ada88")
    (((class color) (min-colors 88) (background dark))
     :background "#001904" :foreground "#b8e2b8" :underline "#006800")
    (t :inherit region))
  "Face used for evaluated forms.")

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
    (read (current-buffer))))

(defun tempel--print-element (elt)
  "Return string representation of template ELT."
  (pcase elt
    ('nil nil)
    (`(q . ,_) nil)
    ((pred stringp) elt)
    (`(s ,name) (symbol-name name))
    (`(,(or 'p 'P) ,_ ,name . ,noinsert)
     (and (not (car noinsert)) (symbol-name name)))
    ((or 'n 'n>) " ")
    (_ "_")))

(defun tempel--expand-locations (locations)
  "Return the list of files sepecified by LOCATIONS.
LOCATIONS is a list of files and directories."
  (let ((files (if (listp locations) locations (list locations))))
    (dolist (file-or-dir locations)
      (if (file-directory-p file-or-dir)
	  (dolist (file (directory-files file-or-dir t))
	    (push file files))
	(push file-or-dir files)))
    (mapcar #'expand-file-name (reverse files))))

(defun tempel--load-templates ()
  "Return templates specified `tempel-template-locations'."
  (mapcan #'tempel--load (tempel--expand-locations tempel-template-locations)))

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

(defun tempel--field-modified (ov after _beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (when after
    (let ((st (overlay-get ov 'tempel--state)))
      (unless undo-in-progress
        (move-overlay ov (overlay-start ov) (max end (overlay-end ov))))
      (tempel--update-mark ov)
      (when-let (name (overlay-get ov 'tempel--name))
        (setf (alist-get name (cdr st))
              (buffer-substring-no-properties
               (overlay-start ov) (overlay-end ov))))
      (unless undo-in-progress
        (template--synchronize-fields st ov)))))

(defun template--synchronize-fields (st current)
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
  "Update field mark from OV."
  (unless (overlay-get ov 'tempel--form)
    (overlay-put ov 'before-string
                 (and (or (= (overlay-start ov) (overlay-end ov))
                          ;; TODO mark for blank fields?
                          ;;(string-blank-p (buffer-substring-no-properties
                          ;;                 (overlay-start ov) (overlay-end ov)))
                          )
                      tempel-mark))))

(defun tempel--field (st &optional name init)
  "Add template field to ST.
NAME is the optional field name.
INIT is the optional initial input."
  (let ((ov (make-overlay (point) (point))))
    (push ov (car st))
    (overlay-put ov 'face 'tempel-field)
    (overlay-put ov 'modification-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-in-front-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-behind-hooks (list #'tempel--field-modified))
    (overlay-put ov 'tempel--state st)
    (when name
      (overlay-put ov 'tempel--name name)
      (setq init (or init (alist-get name (cdr st)) ""))
      (setf (alist-get name (cdr st)) init))
    (when (and init (not (equal init "")))
      (insert init)
      (move-overlay ov (overlay-start ov) (point)))
    (tempel--update-mark ov)))

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

(defun tempel--query (st prompt name)
  "Read input with PROMPT and assign to binding NAME in ST."
  (setf (alist-get name (cdr st)) (if (stringp prompt)
                                      (read-string prompt)
                                    (eval prompt 'lexical-binding))))

(defun tempel--element (st element region)
  "Add template ELEMENT to ST given the REGION."
  (pcase element
    ('nil)
    ('n (insert "\n"))
    ('n> (insert "\n") (indent-according-to-mode))
    ('> (indent-according-to-mode))
    ((pred stringp) (insert element))
    ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
          (insert "\n")))
    ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (insert "\n")))
    ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
	  (open-line 1)))
    ('p (tempel--field st))
    (`(s ,name) (tempel--field st name))
    ;; LEGACY: (r ...) and (r> ...) is legacy syntax from Tempo, use r instead.
    ((or 'r `(r . ,_)) (if region (goto-char (cdr region)) (tempel--field st)))
    ((or 'r> `(r> . ,_)) (if (not region) (tempel--field st)
                           (goto-char (cdr region))
                           (indent-region (car region) (cdr region) nil)))
    ;; LEGACY: (p ...) and (P ...) is legacy syntax from Tempo, use q, s, or p instead.
    ;; TEMPEL EXTENSION: (p (FORM...) <NAME>)
    (`(,(or 'p 'P) ,prompt . ,rest)
     (if (cadr rest)
         (tempel--query st prompt (car rest))
       (tempel--field st (car rest) (and (consp prompt)
                                         (eval prompt 'lexical)))))
    ;; TEMPEL EXTENSION: Query from minibuffer, (q "Prompt: " name), (q (FORM...) name)
    (`(q ,prompt ,name) (tempel--query st prompt name))
    ;; TEMPEL EXTENSION: Evaluate forms
    (_ (tempel--form st element))))

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
      (dolist (x template) (tempel--element st x region))
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
  (let (modified)
    (dolist (file (tempel--expand-locations))
      (when-let (buff (get-file-buffer tempel-file))
  	(push file modified)))
    (when (or (and (= (length modified) 1)
		   (y-or-n-p "Save template file %s? " (car modified)))
	      (y-or-n-p "Save modified template files? ")))
    (mapc #'save-buffer modified)))

(defun tempel--template-files-modified-p ()
  "Return non-nil if any template files have been modified since loading."
  (cl-find-if (lambda (file)
		(let ((mod (file-attribute-modification-time (file-attributes file))))
		  (unless (equal tempel--modified mod)
		    (setq tempel--templates (tempel--load-templates)
			  tempel--modified mod))))
	      (tempel--expand-locations tempel-template-locations)))

(defun tempel--templates ()
  "Return templates for current mode."
  (when (tempel--template-files-modified-p)
    (setq tempel--templates (tempel--load-templates)))
  (let (result (templates tempel--templates))
    (while (and templates (symbolp (car templates)))
      (when (derived-mode-p (car templates))
        (push (seq-take-while #'consp (cdr templates)) result))
      (setq templates (seq-drop-while #'consp (cdr templates))))
    (apply #'append result)))

(defun tempel--region ()
  "Return region bounds."
  (when (use-region-p)
    (when (< (mark) (point)) (exchange-point-and-mark))
    (deactivate-mark)
    (cons (point-marker) (mark-marker))))

(defun tempel--find (dir)
  "Find next overlay in DIR."
  (let ((pt (point)) next)
    (dolist (st tempel--active next)
      (dolist (ov (car st))
        (unless (overlay-get ov 'tempel--form)
          (cond
           ((and (> dir 0) (> (overlay-end ov) pt))
            (setq next (min (or next (point-max)) (overlay-end ov))))
           ((and (< dir 0) (< (overlay-start ov) pt))
            (setq next (max (or next -1) (overlay-start ov))))))))))

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

;;;###autoload
(defun tempel-expand (&optional interactive)
  "Complete template at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (let (completion-cycle-threshold
            (completion-at-point-functions (list #'tempel-expand)))
        (tempel--save)
        (or (completion-at-point) (user-error "Tempel: No completions")))
    (when-let (templates (tempel--templates))
      (let* ((region (tempel--region))
             (bounds (or (and (not region) (bounds-of-thing-at-point 'symbol))
                         (cons (point) (point)))))
        (list (car bounds) (cdr bounds) templates
              :exclusive 'no
              :company-kind (lambda (_) 'snippet)
              :exit-function
              (lambda (name _status)
                (when-let* ((sym (intern-soft name))
                            (template (alist-get sym templates)))
                  (delete-region (max (point-min) (- (point) (length name))) (point))
                  (tempel--insert template region)))
              :annotation-function
              (and tempel-expand-annotation
                   (apply-partially #'tempel--annotate
                                    templates tempel-expand-annotation nil " ")))))))

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
       (defun ,cmd () (interactive) (tempel-insert ',name))
       (define-key ,(or map 'global-map) ,(kbd key) #',cmd))))

;;;###autoload
(defun tempel-define-template ()
  (funcall `(lambda () (tempel-deftemplate ,name ,args))))

;;;###autoload
(defun tempel-deftemplate (name args &rest body)
  "Define a template.
Name is the name of the template."
  (let* ((trigger (car args))
	 (mode (cadr args))
	 (name ()))
    `(progn (defun tempel-insert- ())
	    ())))

(provide 'tempel)
;;; tempel.el ends here
