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



(straight-use-package 'cmake-mode)



(straight-use-package 'csharp-mode)



(straight-use-package 'go-mode)



(straight-use-package 'scribble-mode)



(straight-use-package 'yaml-mode)



(straight-use-package 'racket-mode)
(add-hook 'racket-before-run-hook #'racket-repl-clear)



(progn
  (straight-use-package 'geiser)
  (setq geiser-repl-history-filename (expand-file-name "geiser-history" erica-data-directory))
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))

  (straight-use-package 'geiser-guile)

  (straight-use-package 'geiser-chez)
  (setq geiser-chez-binary "chezscheme")

  (straight-use-package 'geiser-chicken)
  (setq geiser-chicken-binary "csi")

  (defun scheme-module-indent (state indent-point normal-indent) 0)
  (put 'module 'scheme-indent-function 'scheme-module-indent)

  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'let/drop 'scheme-indent-function 1)
  (put 'let/drop-guard 'scheme-indent-function 1)
  (put 'with-memory-pointerof 'scheme-indent-function 1)
  (put 'syntax-table-set! 'scheme-indent-function 1)
  (put 'sharp-syntax-table-set! 'scheme-indent-function 1))



(progn
 (straight-use-package 'eglot)
 (setq eglot-send-changes-idle-time 0.25)
 (setq eglot-autoshutdown t)
 (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
   (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
   (add-hook 'before-save-hook
             (lambda ()
               (when (eglot-managed-p)
                 (call-interactively #'eglot-format-buffer))))))



(straight-use-package 'auctex)
(add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
(setq ConTeXt-Mark-version "IV")
(setq TeX-source-correlate-start-server t)
(setq font-latex-fontify-sectioning 'color)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(with-eval-after-load "context"
  (setq TeX-file-extensions (cons "mkiv" TeX-file-extensions)))
(add-hook 'ConTeXt-mode-hook (lambda () (local-unset-key "\"") (setq TeX-command-default "ConTeXt Full")))
(add-hook 'TeX-after-compilation-finished-functions-hook #'TeX-revert-document-buffer)




(provide 'erica-modes)
