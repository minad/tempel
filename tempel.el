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
(eval-when-compile (require 'subr-x))

(defvar tempel-file (expand-file-name "templates" user-emacs-directory))
(defvar tempel-region nil)
(defvar tempel--templates nil)
(defvar tempel--modified nil)
(defvar tempel--history nil)
(defvar tempel--state nil)
(defvar-local tempel--overlays nil)

(defvar tempel-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right] #'tempel-next-field)
    (define-key map [M-left] #'tempel-previous-field)
    map)
  "Keymap to navigate across template markers.")

(defun tempel--load (file)
  "Load templates from FILE."
  (with-temp-buffer
    (insert "(\n")
    (insert-file-contents-literally file)
    (goto-char (point-max))
    (insert "\n)")
    (goto-char (point-min))
    (let ((templates (read (current-buffer))) result)
      (while (and templates (symbolp (car templates)))
        (push (cons (car templates) (seq-take-while #'consp (cdr templates))) result)
        (setq templates (seq-drop-while #'consp (cdr templates))))
      result)))

(defun tempel--annotate (templates sep name)
  "Annotate template NAME given the list of TEMPLATES and SEP."
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
         (mapconcat (lambda (x) (pcase x
                                  ((pred stringp) x)
                                  ((or 'n 'n>) " ")
                                  (_ "_")))
                    def ""))
        'face 'completions-annotations))
      20 0 ?\s))))

(defun tempel--replace-field (ov str)
  "Replace OV content with STR."
  (let ((inhibit-modification-hooks t)
        (beg (overlay-start ov)))
    (goto-char beg)
    (delete-char (- (overlay-end ov) beg))
    (insert str)
    (move-overlay ov beg (point))))

(defun tempel--update-field (ov after beg end &optional _len)
  "Update field overlay OV.
AFTER is non-nil after the modification.
BEG and END are the boundaries of the modification."
  (when (and after (>= beg (overlay-start ov)) (<= beg (overlay-end ov)))
    (move-overlay ov (overlay-start ov) (max end (overlay-end ov)))
    (when-let (name (overlay-get ov 'tempel--name))
      (let ((state (overlay-get ov 'tempel--state))
            (str (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
        (setf (alist-get name (cddr state)) str)
        (save-excursion
          ;; Update overlays
          (dolist (other (alist-get name (car state)))
            (unless (eq other ov)
              (tempel--replace-field other str)))
          ;; Update forms
          (dolist (other (cadr state))
            (tempel--replace-field other (eval (overlay-get other 'tempel--form) (cddr state)))))))))

(defun tempel--field ()
  "Create template field."
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'face 'highlight)
    (overlay-put ov 'before-string #(" " 0 1 (face highlight display (space :width (1)))))
    (overlay-put ov 'modification-hooks (list #'tempel--update-field))
    (overlay-put ov 'insert-behind-hooks (list #'tempel--update-field))
    (push ov tempel--overlays)
    ov))

(defun tempel--named (name)
  "Create template field named NAME."
  (let ((ov (tempel--field)))
    (push ov (alist-get name (car tempel--state)))
    (overlay-put ov 'tempel--name name)
    (overlay-put ov 'tempel--state tempel--state)
    (when-let (str (alist-get name (cddr tempel--state)))
      (insert str)
      (move-overlay ov (overlay-start ov) (point)))))

(defun tempel--form (form)
  "Create template field evaluating FORM."
  (let ((ov (tempel--field)))
    (push ov (cadr tempel--state))
    (overlay-put ov 'tempel--form form)
    (insert (eval form (cddr tempel--state)))
    (move-overlay ov (overlay-start ov) (point))))

(defun tempel--query (prompt name)
  "Read input with PROMPT and assign to NAME."
  (setf (alist-get name (cddr tempel--state)) (read-string prompt)))

(defun tempel--element (element)
  "Insert template ELEMENT."
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
    ('o (unless (or tempel-region (eolp)
		    (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
	  (open-line 1)))
    ('p (tempel--field))
    ((or 'r `(r . ,_)) (if tempel-region (goto-char (cdr tempel-region)) (tempel--field)))
    ((or 'r> `(r> . ,_))
     (if (not tempel-region) (tempel--field)
       (goto-char (cdr tempel-region))
       (indent-region (car tempel-region) (cdr tempel-region) nil)))
    (`(,(or 'p 'P) ,prompt . ,rest) ;; Tempo legacy, use i, s, or plain p instead
     (cond
      ((cadr rest) (tempel--query prompt (car rest)))
      ((car rest) (tempel--named (car rest)))
      (t (tempel--field))))
    (`(q ,prompt ,name) (tempel--query prompt name)) ;; Tempel Extension over Tempo
    (`(s ,name) (tempel--named name))
    (_ (tempel--form element))))

(defun tempel--insert (templates name)
  "Insert template NAME given the list of TEMPLATES."
  (when-let* ((name (intern-soft name))
              (template (cdr (assoc name templates))))
    (setf (alist-get 'tempel--overlays minor-mode-overriding-map-alist) tempel-map)
    (save-excursion
      ;; Split old overlays
      (dolist (ov tempel--overlays)
        (when (and (<= (overlay-start ov) (point)) (>= (overlay-end ov) (point)))
          (setf (overlay-end ov) (point))))
      ;; Begin marker
      (push (make-overlay (point) (point)) tempel--overlays)
      (let ((tempel--state (list nil nil))
            (inhibit-modification-hooks t))
        (mapc #'tempel--element template))
      ;; End marker
      (push (make-overlay (point) (point)) tempel--overlays))
    (setq tempel--overlays (sort tempel--overlays (lambda (x y) (< (overlay-start x) (overlay-start y)))))
    ;; Jump to first field
    (tempel-next-field 1)))

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
  (cdr (seq-find (lambda (x) (derived-mode-p (car x))) tempel--templates)))

(defun tempel--region ()
  "Return region bounds."
  (when (use-region-p)
    (when (< (mark) (point)) (exchange-point-and-mark))
    (deactivate-mark)
    (cons (point-marker) (mark-marker))))

(defun tempel-next-field (arg)
  "Move ARG fields forward and quit at the end."
  (interactive "p")
  (catch 'tempel--break
    (cond
     ((> arg 0)
      (dolist (ov tempel--overlays)
        (when (> (overlay-start ov) (point))
          (if (> arg 1) (setq arg (1- arg))
            (goto-char (overlay-end ov))
            (throw 'tempel--break nil)))))
     ((< arg 0)
      (dolist (ov (reverse tempel--overlays))
        (when (< (overlay-end ov) (point))
          (if (< arg -1) (setq arg (1+ arg))
            (goto-char (overlay-end ov))
            (throw 'tempel--break nil))))))
    (tempel-done)))

(defun tempel-previous-field (arg)
  "Move ARG fields backward and quit at the beginning."
  (interactive "p")
  (tempel-next-field (- arg)))

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
              :exit-function (lambda (name _status)
                               (delete-region (max (point-min) (- (point) (length name))) (point))
                               (let ((tempel-region region))
                                 (tempel--insert templates name)))
              :annotation-function (apply-partially #'tempel--annotate templates " "))))))

;;;###autoload
(defun tempel-insert ()
  "Insert template using `completing-read'."
  (interactive)
  (let* ((templates (or (tempel--templates)
                        (error "Tempel: No templates for %s" major-mode)))
         (completion-extra-properties
          (list :annotation-function
                (apply-partially #'tempel--annotate templates
                                 #("  " 1 2 (display (space :align-to (+ left 20)))))))
         (name (completing-read "Template: " templates nil t nil 'tempel--history)))
    (let ((tempel-region (tempel--region)))
      (tempel--insert templates name))))

(provide 'tempel)
;;; tempel.el ends here
