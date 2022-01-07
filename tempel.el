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

(defcustom tempel-expand-annotation 20
  "Annotation width for `tempel-expand'."
  :type '(choice (const nil integer)))

(defcustom tempel-elements
  '((nil ignore ignore)
    (n   temple--element-newline " ")
    (n>  temple--element-newline-indent " ")
    (>   temple--element-indent ignore)
    (&   temple--element-newline-start " ")
    (%   temple--element-newline-end " ")
    (o   temple--element-newline-open " ")
    (s   temple--element-named tempel--print-named)
    ;; LEGACY: (r ...) and (r> ...) is legacy syntax from Tempo, use r instead.
    (r   temple--element-region "_")
    (r>  temple--element-region-indent "_")
    ;; LEGACY: (p ...) and (P ...) is legacy syntax from Tempo, use q, s, or p instead.
    ;; TEMPEL EXTENSION: (p (FORM...) <NAME>)
    (p   temple--element-placeholder tempel--print-placeholder)
    (P   temple--element-placeholder tempel--print-placeholder)
    ;; TEMPEL EXTENSION: Query from minibuffer, (q "Prompt: " NAME), (b (FORM...) NAME)
    (q   temple--element-query ignore)
    ;; TEMPEL EXTENSION: Evaluate forms
    (t   temple--element-default "_"))
  "Alist of template elements."
  :type 'alist)

(defface tempel-field '((t :inherit highlight))
  "Face used for fields.")

(defface tempel-form '((t :inherit region))
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

(defvar tempel--state nil
  "State of the current template.
This variable is only valid during dispatching.")

(defvar tempel--region nil
  "Current region bounds.
This variable is only valid during dispatching.")

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

(defun tempel--dispatch-print (elt)
  "Return string representation of template ELT."
  (let ((fun (or (and (stringp elt) elt)
                 (caddr (assq (or (car-safe elt) elt) tempel-elements))
                 (caddr (assq t tempel-elements))
                 (error "Unknown template element %S" elt))))
    (if (stringp fun) fun (funcall fun elt))))

(defun tempel--print-named (elt)
  "Print named template ELT."
  (pcase-exhaustive elt
    (`(s ,name) (symbol-name name))))

(defun tempel--print-placeholder (elt)
  "Print placeholder template ELT."
  (pcase-exhaustive elt
    ((or 'p 'P) "_")
    (`(,(or 'p 'P) ,_ ,name . ,noinsert)
     (and (not (car noinsert)) (symbol-name name)))))

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
                           (mapconcat #'tempel--dispatch-print elts ""))
                          'face 'completions-annotations))
             width 0 ?\s ellipsis))))

(defun tempel--field-modified (ov after _beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (when after
    (let ((st (overlay-get ov 'tempel--state)))
      ;; Update overlay position
      (unless undo-in-progress
        (move-overlay ov (overlay-start ov) (max end (overlay-end ov))))
      (tempel--update-mark ov)
      ;; Update bound value
      (when-let (name (overlay-get ov 'tempel--name))
        (setf (alist-get name (cdr st))
              (buffer-substring-no-properties
               (overlay-start ov) (overlay-end ov))))
      ;; Synchronize other fields
      (unless undo-in-progress
        (dolist (w (car st))
          (unless (eq w ov)
            (save-excursion
              (goto-char (overlay-start w))
              (let (x)
                (setq x (or (and (setq x (overlay-get w 'tempel--form)) (eval x (cdr st)))
                            (and (setq x (overlay-get w 'tempel--name)) (alist-get x (cdr st)))))
                (when x (tempel--replace (overlay-start w) (overlay-end w) w x))))))))))

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

(defun tempel--insert-field (&optional name init)
  "Insert editable template field.
NAME is the optional field name.
INIT is the optional initial input."
  (let ((ov (make-overlay (point) (point))))
    (push ov (car tempel--state))
    (overlay-put ov 'face 'tempel-field)
    (overlay-put ov 'modification-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-in-front-hooks (list #'tempel--field-modified))
    (overlay-put ov 'insert-behind-hooks (list #'tempel--field-modified))
    (overlay-put ov 'tempel--state tempel--state)
    (when name
      (overlay-put ov 'tempel--name name)
      (setq init (or init (alist-get name (cdr tempel--state)) ""))
      (setf (alist-get name (cdr tempel--state)) init))
    (when (and init (not (equal init "")))
      (insert init)
      (move-overlay ov (overlay-start ov) (point)))
    (tempel--update-mark ov)))

(defun tempel--query (prompt name)
  "Read input with PROMPT and bind to NAME."
  (setf (alist-get name (cdr tempel--state))
        (if (stringp prompt)
            (read-string prompt)
          (eval prompt 'lexical-binding))))

(defun temple--element-newline (_elt)
  "Insert newline."
  (insert "\n"))

(defun temple--element-newline-indent (_elt)
  "Insert newline and indent according to mode."
  (insert "\n")
  (indent-according-to-mode))

(defun temple--element-indent (_elt)
  "Indent according to mode."
  (indent-according-to-mode))

(defun temple--element-newline-start (_elt)
  "Insert newline if there is only whitespace between line start and point."
  (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
    (insert "\n")))

(defun temple--element-newline-end (_elt)
  "Insert newline if there is only whitespace between point and line end."
  (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
    (insert "\n")))

(defun temple--element-newline-open (_elt)
  "Insert open if there is only whitespace between point and line end."
  (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
    (open-line 1)))

(defun temple--element--placeholder (elt)
  "Handle placeholder ELT."
  (pcase-exhaustive elt
    ('p (tempel--insert-field))
    (`(,(or 'p 'P) ,prompt . ,rest)
     (if (cadr rest)
         (tempel--query prompt (car rest))
       (tempel--insert-field (car rest) (and (consp prompt)
                                             (eval prompt 'lexical)))))))

(defun temple--element-named (elt)
  "Handle named placeholder ELT."
  (pcase-exhaustive elt
    (`(s ,name) (tempel--insert-field name))))

(defun temple--element-region (elt)
  "Handle region ELT."
  (if tempel--region
      (goto-char (cdr tempel--region))
    (tempel--insert-field))
  (pcase-exhaustive elt
    ((or 'r `(r . ,_)))
    ((and (or 'r> `(r> . ,_)) (guard tempel--region))
     (indent-region (car tempel--region) (cdr tempel--region) nil))))

(defun temple--element-query (elt)
  "Handle query ELT."
  (pcase-exhaustive elt
    (`(q ,prompt ,name) (tempel--query prompt name))))

(defun temple--element-default (form)
  "Evaluate FORM and create insert dynamic template field."
  (let ((beg (point)))
    (condition-case nil
        (insert (eval form (cdr tempel--state)))
      ;; Ignore errors since some variables may not be defined yet.
      (void-variable nil))
    (let ((ov (make-overlay beg (point) nil t)))
      (overlay-put ov 'face 'tempel-form)
      (overlay-put ov 'tempel--form form)
      (push ov (car tempel--state)))))

(defun tempel--dispatch (elt)
  "Dispatch template element ELT."
  (if (stringp elt) (insert elt)
    (funcall (or (cadr (assq (or (car-safe elt) elt) tempel-elements))
                 (cadr (assq t tempel-elements))
                 (error "Unknown template element %S" elt))
             elt)))

(defun tempel--insert (templates name region)
  "Insert template NAME given the list of TEMPLATES and the current REGION."
  (when-let* ((name (intern-soft name))
              (template (cdr (assoc name templates))))
    (tempel--enable template region)))

(defun tempel--enable (template region)
  "Enable TEMPLATE given the current REGION."
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
    ;; Interpret template
    (let ((tempel--state (cons nil nil))
          (tempel--region region)
          (inhibit-modification-hooks t))
      (push (make-overlay (point) (point)) (car tempel--state))
      (overlay-put (caar tempel--state) 'face 'cursor) ;; TODO debug
      (mapc #'tempel--dispatch template)
      (push (make-overlay (point) (point) nil t t) (car tempel--state))
      (overlay-put (caar tempel--state) 'face 'cursor) ;; TODO debug
      (push tempel--state tempel--active)))
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
  (let ((mod (file-attribute-modification-time (file-attributes tempel-file))))
    (unless (equal tempel--modified mod)
      (setq tempel--templates (tempel--load tempel-file)
            tempel--modified mod)))
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
                (delete-region (max (point-min) (- (point) (length name))) (point))
                (tempel--insert templates name region))
              :annotation-function
              (and tempel-expand-annotation
                   (apply-partially #'tempel--annotate
                                    templates tempel-expand-annotation nil " ")))))))

;;;###autoload
(defun tempel-insert ()
  "Insert template using `completing-read'."
  (interactive)
  (let* ((templates (or (tempel--templates)
                        (error "Tempel: No templates for %s" major-mode)))
         (completion-extra-properties
          (and tempel-insert-annotation
               (list :annotation-function
                     (apply-partially
                      #'tempel--annotate templates tempel-insert-annotation t
                      #("  " 1 2 (display (space :align-to (+ left 20))))))))
         (name (completing-read "Template: " templates nil t nil 'tempel--history)))
    (tempel--insert templates name (tempel--region))))

(provide 'tempel)
;;; tempel.el ends here
