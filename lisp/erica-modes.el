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



(straight-use-package 'racket-mode)
(add-hook 'racket-before-run-hook #'racket-repl-clear)



(progn
  (straight-use-package 'geiser)
  (straight-use-package 'geiser-chez)
  (straight-use-package 'geiser-chicken)
  (setq geiser-chicken-binary "csi")
  (setq geiser-chez-binary "chezscheme")
  (straight-use-package 'geiser-guile)
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))

  (defun scheme-module-indent (state indent-point normal-indent) 0)
  (put 'module 'scheme-indent-function 'scheme-module-indent)

  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)

  (put 'syntax-table-set! 'scheme-indent-function 1)
  (put 'sharp-syntax-table-set! 'scheme-indent-function 1))


(straight-use-package 'eglot)
(setq eglot-send-changes-idle-time 0.5)
(setq eglot-events-buffer-size nil)
;; (dolist (server '(("csharp-mode" . ("/usr/bin/omnisharp" "-lsp"))))
;;   (add-to-list 'eglot-server-programs (cons (make-symbol (car server)) (cdr server)))
;;   (add-hook (make-symbol (concat (car server) "-hook")) #'eglot-ensure))



(straight-use-package 'auctex)
(add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
(setq ConTeXt-Mark-version "IV")
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(with-eval-after-load "context"
  (setq TeX-file-extensions (cons "mkiv" TeX-file-extensions)))
(add-hook 'ConTeXt-mode-hook (lambda () (local-unset-key "\"") (setq TeX-command-default "ConTeXt Full")))
(add-hook 'TeX-after-compilation-finished-functions-hook #'TeX-revert-document-buffer)



(provide 'erica-modes)
