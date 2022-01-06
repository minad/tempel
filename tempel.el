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
;; configuration. You can jump with the keys M-up/down from field to
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

(defcustom tempel-insert-annotation 40
  "Annotation width for `tempel-insert'."
  :type '(choice (const nil integer)))

(defcustom tempel-expand-annotation 20
  "Annotation width for `tempel-expand'."
  :type '(choice (const nil integer)))

(defface tempel-field '((t :inherit highlight))
  "Face used for fields.")

(defface tempel-form '((t :inherit region))
  "Face used for evaluated forms.")

(defvar tempel--templates nil)
(defvar tempel--modified nil)
(defvar tempel--history nil)
(defvar tempel--state nil)
(defvar-local tempel--overlays nil)

(defvar tempel-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right] #'tempel-next)
    (define-key map [M-left] #'tempel-previous)
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

(defun tempel--annotate (templates width ellipsis sep name)
  "Annotate template NAME given the list of TEMPLATES.
WIDTH, SEP and ELLIPSIS configure the formatting."
  (when-let* ((name (intern-soft name))
              (def (cdr (assoc name templates))))
    (concat
     sep
     (truncate-string-to-width
      (replace-regexp-in-string
       "_+" #("_" 0 1 (face shadow))
       (propertize
        (replace-regexp-in-string
         "\\s-+" " "
         (mapconcat (lambda (x)
                      (pcase x
                        ('nil nil)
                        (`(q . ,_) nil)
                        ((pred stringp) x)
                        (`(s ,name) (symbol-name name))
                        (`(,(or 'p 'P) ,_ ,name . ,noinsert)
                         (and (not (car noinsert)) (symbol-name name)))
                        ((or 'n 'n>) " ")
                        (_ "_")))
                    def ""))
        'face 'completions-annotations))
      width 0 ?\s ellipsis))))

(defun tempel--replace-field (ov str)
  "Replace OV content with STR."
  (save-excursion
    (let ((beg (overlay-start ov)))
      (goto-char beg)
      (delete-char (- (overlay-end ov) beg))
      (insert str)
      (move-overlay ov beg (point)))))

(defun tempel--update-field (ov after beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (when (and after (>= beg (overlay-start ov)) (<= beg (overlay-end ov)))
    (save-excursion
      ;; TODO: We use silent modifications such that the undo list is
      ;; not affected and that the modification hooks are not triggered
      ;; recursively. However since the edits which are made to the
      ;; other fields are not recorded in the undo list, we will end up
      ;; with an invalid undo state after tempel-done.
      (with-silent-modifications
        (move-overlay ov (overlay-start ov) (max end (overlay-end ov)))
        (let ((state (overlay-get ov 'tempel--state)))
          ;; Update other named fields
          (when-let (name (overlay-get ov 'tempel--name))
            (let ((str (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
              (setf (alist-get name (cddr state)) str)
              (dolist (other (alist-get name (car state)))
                (unless (eq other ov)
                  (tempel--replace-field other str)))))
          ;; Update evaluated form fields
          (dolist (other (cadr state))
            (tempel--replace-field
             other
             (save-excursion
               (goto-char (overlay-start other))
               (eval (overlay-get other 'tempel--form) (cddr state))))))))))

(defun tempel--field (&optional face)
  "Create template field with FACE."
  (let ((ov (make-overlay (point) (point))))
    (setq face (or face 'tempel-field))
    (overlay-put ov 'face face)
    (overlay-put ov 'before-string (propertize " " 'face face 'display '(space :width (1))))
    (overlay-put ov 'modification-hooks (list #'tempel--update-field))
    (overlay-put ov 'insert-behind-hooks (list #'tempel--update-field))
    (overlay-put ov 'tempel--state tempel--state)
    (push ov tempel--overlays)
    ov))

(defun tempel--named (name)
  "Create template field named NAME."
  (let ((ov (tempel--field)))
    (overlay-put ov 'tempel--name name)
    (push ov (alist-get name (car tempel--state)))
    (when-let (str (alist-get name (cddr tempel--state)))
      (insert str)
      (move-overlay ov (overlay-start ov) (point)))))

(defun tempel--form (form)
  "Create template field evaluating FORM."
  (let ((ov (tempel--field 'tempel-form)))
    (overlay-put ov 'tempel--form form)
    (push ov (cadr tempel--state))
    ;; Ignore variable errors, since some variables may not be defined yet.
    (condition-case nil
        (insert (eval form (cddr tempel--state)))
      (void-variable nil))
    (move-overlay ov (overlay-start ov) (point))))

(defun tempel--query (prompt name)
  "Read input with PROMPT and assign to NAME."
  (setf (alist-get name (cddr tempel--state)) (read-string prompt)))

(defun tempel--element (element region)
  "Insert template ELEMENT given the REGION."
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
    ('o (unless (or region (eolp)
		    (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
	  (open-line 1)))
    ('p (tempel--field))
    (`(s ,name) (tempel--named name))
    ;; LEGACY: (r ...) and (r> ...) is legacy syntax from Tempo, use r instead.
    ((or 'r `(r . ,_)) (if region (goto-char (cdr region)) (tempel--field)))
    ((or 'r> `(r> . ,_))
     (if (not region) (tempel--field)
       (goto-char (cdr region))
       (indent-region (car region) (cdr region) nil)))
    ;; LEGACY: (p ...) and (P ...) is legacy syntax from Tempo, use q, s, or p instead.
    (`(,(or 'p 'P) ,prompt . ,rest)
     (cond
      ((cadr rest) (tempel--query prompt (car rest)))
      ((car rest) (tempel--named (car rest)))
      (t (tempel--field))))
    ;; EXTENSION: Query from minibuffer, Tempel extension!
    (`(q ,prompt ,name) (tempel--query prompt name))
    ;; EXTENSION: Evaluate forms, Tempel extension!
    (_ (tempel--form element))))

(defun tempel--insert (templates name region)
  "Insert template NAME given the list of TEMPLATES and the REGION."
  (when-let* ((name (intern-soft name))
              (template (cdr (assoc name templates))))
    (setf (alist-get 'tempel--overlays minor-mode-overriding-map-alist) tempel-map)
    (unless (eq buffer-undo-list t)
      (push (list 'apply #'tempel-done) buffer-undo-list))
    (save-excursion
      ;; Split existing overlays, do not expand within existing field.
      (dolist (ov tempel--overlays)
        (when (and (<= (overlay-start ov) (point)) (>= (overlay-end ov) (point)))
          (setf (overlay-end ov) (point))))
      ;; Begin marker
      (push (make-overlay (point) (point)) tempel--overlays)
      (let ((tempel--state (list nil nil))
            (inhibit-modification-hooks t))
        (dolist (x template) (tempel--element x region)))
      ;; End marker
      (push (make-overlay (point) (point)) tempel--overlays))
    ;; Jump to first field
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
  (let ((pt (point))
        (next nil))
    (if (> dir 0)
        (cl-loop for ov in tempel--overlays do
                 (when (and (not (overlay-get ov 'tempel--form)) ;; Skip form
                            (> (overlay-start ov) pt))
                   (setq next (min (or next most-positive-fixnum) (overlay-end ov)))))
      (cl-loop for ov in tempel--overlays do
               (when (and (not (overlay-get ov 'tempel--form)) ;; Skip form
                          (< (overlay-end ov) pt))
                 (setq next (max (or next most-negative-fixnum) (overlay-end ov))))))
    next))

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

(defun tempel-done ()
  "Template completion is done."
  (interactive)
  (mapc #'delete-overlay tempel--overlays)
  (setq tempel--overlays nil
        minor-mode-overriding-map-alist
        (delq (assq-delete-all 'tempel--overlays minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

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
                   (apply-partially #'tempel--annotate templates tempel-expand-annotation nil " ")))))))

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
