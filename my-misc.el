;;; my-misc.el --- my miscellaneous elisp scripts -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Version: 0.0.1
;; Package-Requires: none
;; Homepage: none
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

;;;###autoload
(defun my:unfill-paragraph (&optional region)
  "Make a multi-paragraph REGION into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun my:eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its VALUE."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

;;;###autoload
(defun my:dedicate-window-toggle ()
  "Toggle sticky buffer to current window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))

;;;###autoload
(defun my:asset-directory (&optional output-file)
  "Ensure & return path <buffer-file-name>.assets/OUTPUT-FILE.
This can be used as the :output argument for org code blocks."
  (let ((asset-dir (concat (file-name-sans-extension
                            (buffer-file-name))
                           ".assets")))
    (unless (file-exists-p asset-dir)
      (mkdir asset-dir))
    (concat (file-name-as-directory asset-dir)
            output-file)))

;;;###autoload
(defun my:loaddefs-regenerate (dir &optional generate-full)
  "Regenerate loaddefs for given DIR.
Universal argument GENERATE-FULL is passed to `loaddefs-generate'."
  (interactive "DPath of pacakge: \nP")
  (let ((output
         (concat (file-name-as-directory dir)
                 (replace-regexp-in-string
                  "\\(\\.el\\|-[0-9]\\{1,8\\}\\(\\.[0-9]+\\)\\)+$" ""
                  (file-name-nondirectory (directory-file-name dir)))
                 "-autoloads.el")))
    (loaddefs-generate dir output nil nil nil generate-full)))

;;;###autoload
(defun my:loaddefs-regenerate-subdirs (dir &optional all-in-one)
  "Regenerate loaddefs for all subdirectories under DIR.
When universal argument ALL-IN-ONE is given, compose them into
one single `loaddefs.el' and byte-compile it."
  (interactive "DPath of parent directory: \nP")
  (dolist (child (file-name-all-completions "" dir))
    (when (and (directory-name-p child)
               (not (member child '("./" "../" ".git/" "archives/" "gnupg/"))))
      (message "Regenerate loaddefs for %s ..." child)
      (let ((subdir (concat (file-name-as-directory dir) child)))
        (when all-in-one
          (let ((all-in-file
                 (concat (file-name-as-directory dir) "loaddefs.el")))
            (when (file-exists-p all-in-file)
              (delete-file all-in-file))
            (loaddefs-generate subdir all-in-file)
            (byte-compile-file all-in-file))
          (my:loaddefs-regenerate subdir t))))))

;;;###autoload
(defun my:byte-compile-subdirs (dir)
  "Byte compile all subdirectories under DIR.

There is also an async version provided by koishimacs, which
depends on package `async' ."
  (interactive "DPath of parent directory: ")
  (dolist (file (file-name-all-completions "" dir))
    (when (and (directory-name-p file)
               (not (member file '("./" "../" ".git/" "archives/" "gnupg/"))))
      (byte-recompile-directory
       (concat (file-name-as-directory dir) file)))))

;;;###autoload
(defun my:byte-force-recompile-subdirs (dir)
  "Call `byte-force-recompile' on all subdirectories of DIR.

There is also an async version provided by koishimacs, which
depends on package `async' ."
  (interactive "DPath of parent directory: ")
  (dolist (child (file-name-all-completions "" dir))
    (when (and (directory-name-p child)
               (not (member child '("./" "../" ".git/" "archives/" "gnupg/"))))
      (byte-force-recompile
       (concat (file-name-as-directory dir) child)))))

;;;###autoload
(defun my:adjust-alpha (fg bg)
  "Set window opacity to (FG . BG) by setting frame parameter `alpha'."
  (declare (obsolete my:adjust-alpha-background "29"))
  (interactive
   "nForeground Transparency Value 0 - 100 opaque: \n\
nBackground Transparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha `(,fg . ,bg)))

;;;###autoload
(defun my:adjust-alpha-background (value)
  "Set window opacity to VALUE by setting frame parameter `alpha-background' ."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha-background value))

(provide 'my-misc)
;;; my-misc.el ends here
