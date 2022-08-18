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



(use-package project
  :straight nil
  :custom
  (project-vc-merge-submodules nil))

(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-laGh1v --group-directories-first")
  :config
  (defalias 'dired-find-file 'dired-find-alternate-file)
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window))))

(use-package compile
  :straight nil
  :commands (compile recompile)
  :bind ("<f5>" . recompile)
  :custom
  (compile-command "make -Cbuild -j9 --no-print-directory")
  :hook
  (compilation-mode . (lambda () (setq-local truncate-lines nil))))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t))

(use-package rg
  :bind ("C-x p g" . rg-project)
  :config
  (add-to-list 'rg-custom-type-aliases '("el" . "*.el"))
  (add-to-list 'rg-custom-type-aliases '("ss" . "*.ss *.scm *.sls *.sld")))

(use-package magit
  :commands magit
  :config
  (magit-auto-revert-mode -1))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun erica-setup-nov-mode ()
    (setq-local fill-column 140)
    (setq-local cursor-type nil)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (face-remap-add-relative 'variable-pitch '(:family "IBM Plex Serif")))
  :custom
  (nov-text-width 120)
  (nov-save-place-file (expand-file-name "nov-places" erica-data-directory))
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*outline"
                 display-buffer-in-side-window
                 (side . left)
                 (window-width . 0.35)
                 (inhibit-switch-frame . t)))
  :hook
  (nov-mode . erica-setup-nov-mode))



(provide 'erica-tools)

