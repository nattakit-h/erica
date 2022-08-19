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



(with-eval-after-load 'project
  (defvar project-vc-merge-submodules nil))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
  (defalias 'dired-find-file 'dired-find-alternate-file))

(with-eval-after-load 'compile
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window)))
  (add-hook 'compilation-mode (lambda () (setq-local truncate-lines nil))))

(defvar magit-auto-revert-mode nil)
(straight-use-package 'magit)

(straight-use-package 'magit-todos)
(add-hook 'magit-status-mode-hook #'magit-todos-mode)

;; (straight-use-package 'pdf-tools)
;; (pdf-tools-install t)
;; (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
;; (with-eval-after-load 'pdf-tools
;;   (pdf-tools-install t))

;; (use-package rg
;;   :defer t
;;   ;; :after (project)
;;   :bind ("C-x p g" . rg-project)
;;   :config
;;   (add-to-list 'rg-custom-type-aliases '("el" . "*.el"))
;;   (add-to-list 'rg-custom-type-aliases '("ss" . "*.ss *.scm *.sls *.sld")))

;; (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
;; (pp (macroexpand '(use-package pdf-tools
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install t))))

;; (use-package nov
;;   :mode ("\\.epub\\'" . nov-mode)
;;   :custom
;;   (nov-text-width 120)
;;   (nov-save-place-file (expand-file-name "nov-places" erica-data-directory))
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*outline"
;;                  display-buffer-in-side-window
;;                  (side . left)
;;                  (window-width . 0.35)
;;                  (inhibit-switch-frame . t)))
;;   :hook
;;   (nov-mode . (lambda ()
;;                 (use-package visual-fill-column
;;                   :custom
;;                   (visual-fill-column-center-text t))

;;                 (setq-local fill-column 140)
;;                 (setq-local cursor-type nil)
;;                 (visual-line-mode 1)
;;                 (visual-fill-column-mode 1)
;;                 (face-remap-add-relative 'variable-pitch '(:family "IBM Plex Serif")))))



(provide 'erica-tools)

