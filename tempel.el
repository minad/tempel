;;; tempel.el --- Tempo Templates -*- lexical-binding: t -*-

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

;; This package provides a modern user interface to the Emacs Tempo
;; template library. Your templates are stored in the `tempel-file' (by
;; default the file "templates" in the `user-emacs-directory'). Bind the
;; commands `tempel-expand' or `tempel-insert' to some keys in your user
;; configuration. You can jump with the keys M-up/down from field to
;; field. `tempel-expands' works best with the Corfu completion UI,
;; while `temple-insert' uses `completing-read' under the hood.

;;; Code:

(require 'tempo)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defvar tempel-file (expand-file-name "templates" user-emacs-directory))
(defvar tempel--templates nil)
(defvar tempel--modified nil)
(defvar tempel--current nil)
(defvar tempel--history nil)

(defvar tempel-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right] #'tempel-forward-mark)
    (define-key map [M-left] #'tempel-backward-mark)
    map)
  "Keymap to navigate across template markers.")

(defun tempel--load (file)
  "Load Tempo templates from FILE."
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
         (mapconcat (lambda (x)
                      (pcase x
                        ((pred stringp) x)
                        ((or 'n 'n>) " ")
                        (_ "_")))
                    def ""))
        'face 'completions-annotations))
      20 0 ?\s))))

(defun tempel--insert (templates region name)
  "Insert template NAME given the list of TEMPLATES and REGION flag."
  (when-let* ((name (intern-soft name))
              (template (cdr (assoc name templates))))
    (setf (alist-get 'tempo-marks minor-mode-overriding-map-alist) tempel-map)
    (let ((tempel--current template))
      (setq tempo-marks nil)
      (tempo-insert-template 'tempel--current region))))

(defun tempel--save ()
  "Save Tempo file buffer."
  (when-let (buf (get-file-buffer tempel-file))
    (with-current-buffer buf
      (when (and (buffer-modified-p) (y-or-n-p (format "Save file %s? " tempel-file)))
        (save-buffer buf)))))

(defun tempel--templates ()
  "Return Tempo templates for current mode."
  (let ((mod (file-attribute-modification-time (file-attributes tempel-file))))
    (unless (equal tempel--modified mod)
      (setq tempel--templates (tempel--load tempel-file)
            tempel--modified mod)))
  (cdr (seq-find (lambda (x) (derived-mode-p (car x))) tempel--templates)))

(defun tempel-forward-mark ()
  "Move to next template mark and quit at the end."
  (interactive)
  (tempo-forward-mark)
  (when-let (mark (car (last tempo-marks)))
    (when (>= (point) mark) (tempel-done))))

(defun tempel-backward-mark ()
  "Move to previous template mark and quit at the beginning."
  (interactive)
  (tempo-backward-mark)
  (when-let (mark (car tempo-marks))
    (when (<= (point) mark) (tempel-done))))

(defun tempel-done ()
  "Template completion is done."
  (interactive)
  (dolist (mark tempo-marks) (set-marker mark nil))
  (setq tempo-marks nil
        minor-mode-overriding-map-alist
        (delq (assq-delete-all 'tempo-marks minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

;;;###autoload
(defun tempel-expand (&optional interactive)
  "Complete Tempo template at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (let (completion-cycle-threshold
            (completion-at-point-functions (list #'tempel-expand)))
        (tempel--save)
        (or (completion-at-point) (user-error "Tempel: No completions")))
    (tempel-done)
    (when-let (templates (tempel--templates))
      (let ((region (use-region-p)) bounds)
        (when region
          (when (< (mark) (point)) (exchange-point-and-mark))
          (deactivate-mark))
        (setq bounds
              (or (and (not region) (bounds-of-thing-at-point 'symbol))
                  (cons (point) (point))))
        (list (car bounds)
              (cdr bounds)
              templates
              :exclusive 'no
              :exit-function (lambda (name _status)
                               (delete-region (max (point-min) (- (point) (length name))) (point))
                               (tempel--insert templates region name))
              :annotation-function (apply-partially #'tempel--annotate templates " "))))))

;;;###autoload
(defun tempel-insert ()
  "Insert Tempo template using `completing-read'."
  (interactive)
  (tempel-done)
  (let* ((templates (or (tempel--templates)
                        (error "Tempel: No templates for %s" major-mode)))
         (completion-extra-properties
          (list :annotation-function
                (apply-partially #'tempel--annotate templates
                                 #("  " 1 2 (display (space :align-to (+ left 20)))))))
         (name (completing-read "Template: " templates nil t nil 'tempel--history)))
    (tempel--insert templates (use-region-p) name)))

(provide 'tempel)
;;; tempel.el ends here
