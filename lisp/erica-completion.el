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



(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))


(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode 1))

;; (progn
;;  (straight-use-package 'consult)
;;  (keymap-global-set "C-x b" #'consult-buffer)
;;  (keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
;;  (keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)
;;  (keymap-global-set "C-x p b" #'consult-project-buffer)
;;  (add-hook 'completion-list-mode #'consult-preview-at-point-mode)
;;  (setq xref-show-xrefs-function #'consult-xref
;;        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless)))



(provide 'erica-completion)

