;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Nattakit Hosapsin <nattakit@hosapsin.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(setq project-vc-merge-submodules nil)
(setq dired-listing-switches "-laGh1v --group-directories-first")



(keymap-global-set "<f5>" #'recompile)
(setq compilation-scroll-output 'first-error)
(defvar erica-compilation-auto-delete nil)
(add-hook 'compilation-finish-functions
  (lambda (buf str)
    (when (and erica-compilation-auto-delete
               (null (string-match ".*exited abnormally.*" str)))
      (delete-windows-on (get-buffer-create "*compilation*")))))



(defun erica-eshell-clear ()
  (interactive "" '(eshell-mode))
  (end-of-buffer)
  (eshell-kill-input)
  (insert "clear 1")
  (eshell-send-input)
  (end-of-buffer)
  (eshell-bol)
  (yank))

(defun erica-eshell-hook () (local-set-key (kbd "C-c M-o") #'erica-eshell-clear))
(add-hook 'eshell-mode-hook #'erica-eshell-hook)



(straight-use-package 'rg)
(rg-enable-default-bindings)



(straight-use-package 'visual-fill-column)
(setq visual-fill-column-center-text t)



(straight-use-package 'magit)
(magit-auto-revert-mode -1)



(straight-use-package 'pdf-tools)
(pdf-tools-install)



(progn
  (straight-use-package 'nov)
  (setq nov-text-width 120)
  (setq nov-save-place-file (expand-file-name "nov-places" erica-data-directory))
  (add-to-list 'display-buffer-alist
               '("^\\*outline"
                 display-buffer-in-side-window
                 (side . left)
                 (window-width . 0.35)
                 (inhibit-switch-frame . t)))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun erica-setup-nov-mode ()
    (setq-local fill-column 140)
    (setq-local cursor-type nil)
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  (add-hook 'nov-mode-hook #'erica-setup-nov-mode))



(provide 'erica-tools)

