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



(setq use-package-verbose t)

(defvar bootstrap-version)
(setq straight-use-package-by-default t)
(setq straight-base-dir erica-data-directory)
(setq straight-check-for-modifications '(check-on-save))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight)


(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(require 'bind-key)


(use-package no-littering
  :init
  (setq no-littering-var-directory erica-data-directory)
  (setq no-littering-etc-directory erica-config-directory)
  (setq project-list-file (expand-file-name "projects.el" erica-data-directory))
  (setq custom-file (expand-file-name "custom.el" erica-config-directory))
  :config
  (when (file-exists-p custom-file)
    (load-file custom-file)))

(use-package exec-path-from-shell
  :disabled
  :custom
  (path-from-shell-arguments (remove "-i" exec-path-from-shell-arguments))
  :config
  (exec-path-from-shell-initialize))




(provide 'erica-init)
