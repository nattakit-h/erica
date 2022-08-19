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



(use-package glsl-mode
  :defer t)

(use-package cmake-mode
  :defer t)

;; (use-package csharp-mode
;;   :defer t)

;; (use-package go-mode
;;   :defer t)

;; (use-package yaml-mode
;;   :defer t)

(use-package racket-mode
  :defer t
  :hook
  (racket-before-run . racket-repl-clear))

;; (use-package scribble-mode
;;   :defer t)

;; (use-package geiser
;;   :defer t
;;   :custom
;;   (geiser-repl-history-filename (expand-file-name "geiser-history" erica-data-directory))
;;   :config
;;   (put 'module 'scheme-indent-function 0)
;;   (put 'and-let* 'scheme-indent-function 1)
;;   (put 'parameterize 'scheme-indent-function 1)
;;   (put 'handle-exceptions 'scheme-indent-function 1)
;;   (put 'when 'scheme-indent-function 1)
;;   (put 'unless 'scheme-indent-function 1)
;;   (put 'match 'scheme-indent-function 1))

;; (use-package geiser-chez
;;   :after (geiser)
;;   :custom
;;   (geiser-chez-binary "chezscheme"))

;; (use-package eglot
;;   :bind ("<f8>" . eglot)
;;   :custom
;;   (eglot-send-changes-idle-time 0.25)
;;   (eglot-autoshutdown t)
;;   :config
;;   (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
;;   (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
;;   :hook
;;   (before-save . (lambda () (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
;;                               (call-interactively #'eglot-format-buffer)))))

;; (straight-use-package 'auctex)
;; (add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
;; (setq ConTeXt-Mark-version "IV")
;; (setq TeX-source-correlate-start-server t)
;; (setq font-latex-fontify-sectioning 'color)
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
;; (with-eval-after-load "context"
;;   (setq TeX-file-extensions (cons "mkiv" TeX-file-extensions)))
;; (add-hook 'ConTeXt-mode-hook (lambda () (local-unset-key "\"") (setq TeX-command-default "ConTeXt Full")))
;; (add-hook 'TeX-after-compilation-finished-functions-hook #'TeX-revert-document-buffer)




(provide 'erica-modes)
