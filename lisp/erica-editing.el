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



(delete-selection-mode 1)
(electric-pair-mode 1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)

(setq save-silently t)
(setq savehist-file (expand-file-name "history" erica-data-directory))
(setq recentf-save-file (expand-file-name "recentf" erica-data-directory))
(setq global-auto-revert-non-file-buffers t)
(setq kill-do-not-save-duplicates t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; (defun erica-ignore-save-scratch ()
;;   (when (string-equal (buffer-name) "*scratch*")
;;     (keyboard-quit)))

;; (advice-add 'quit-window :filter-args
;;             (lambda (args)
;;               (pcase args
;;                 (`() '(t))
;;                 (`(,kill) `(,(not kill)))
;;                 (`(,kill ,window) `(,(not kill) ,window)))))
;; (add-hook 'before-save-hook #'erica-ignore-save-scratch)
;; (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(c-add-style "cc" '("bsd" (c-basic-offset . 4) (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "c" '("bsd" (c-basic-offset . 4)))
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (c++-mode . "cc") (other . "c")))
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; (use-package hungry-delete
;;   :custom
;;   (hungry-delete-chars-to-skip " \t")
;;   :config
;;   (global-hungry-delete-mode 1))

;; (use-package visual-regexp
;;   :config
;;   (defalias 'replace-regexp #'vr/replace)
;;   (defalias 'query-replace-regexp #'vr/query-replace))


;; (use-package yasnippet-snippets
;;   :defer t)

;; (use-package yasnippet
;;   :after (yasnippet-snippets)
;;   :hook
;;   ((c-modfe cmake-mode) . yas-minor-mode))



(provide 'erica-editing)
