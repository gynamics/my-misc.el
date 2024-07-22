;;; my-misc.el --- my miscellaneous elisp scripts -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 0.1
;; Package-Requires:
;; URL: https://github.com/gynamics/my-misc.el
;; Keywords: ultility


;; This file is not part of GNU Emacs

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

;; Here are some useful short functions I collected that are not
;; shipped with Emacs, all started with a `my:` prefix.

;;; Code:

(defun my:unfill-paragraph (&optional region)
  "Make a multi-paragraph REGION into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my:eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its VALUE."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun my:dedicate-window-toggle ()
  "Toggle sticky buffer to current window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))

;; This can be used as the :output argument for org-babel
(defun my:asset-directory (&optional output-file)
  "Ensure & return path <buffer-file-name>.assets/OUTPUT-FILE."
  (let ((asset-dir (concat (file-name-sans-extension
                            (buffer-file-name))
                           ".assets")))
    (unless (file-exists-p asset-dir)
      (mkdir asset-dir))
    (concat (file-name-as-directory asset-dir) output-file)
    ))

(defun my:make-autoloads-subdirectories (dir)
  "Make autoloads for all subdirectories under DIR."
  (interactive "DPath of parent directory: ")
  (dolist (file (file-name-all-completions "" dir))
    (when (and (directory-name-p file)
               (not (member file '("./" "../" "archives/" "gnupg/"))))
      (let ((output-file
             (concat dir file
                     (replace-regexp-in-string
                      "-[0-9]\\{1,8\\}\\(\\.[0-9]+\\)+/" "" file)
                     "-autoloads.el"))
            )
        (loaddefs-generate (concat dir file) output-file)
        (byte-compile-file output-file)))))

(defun my:adjust-alpha (fg bg)
  "Set window opacity to (FG . BG) by setting frame parameter alpha."
  (interactive
   "nForeground Transparency Value 0 - 100 opaque: \n\
nBackground Transparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha `(,fg . ,bg)))

(make-obsolete #'my:adjust-alpha "my:adjust-alpha-background,"
               (> emacs-major-version 28))

(defun my:adjust-alpha-background (value)
  "Set window opacity to VALUE by setting frame parameter alpha-background."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha-background value))

(provide 'my-misc)
;;; my-misc.el ends here
