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

;; Standard directories

(defmacro erica-user-subdirectory (name)
  `(expand-file-name ,(symbol-name name) user-emacs-directory))
(defconst erica-lisp-directory (erica-user-subdirectory lisp))
(defconst erica-config-directory (erica-user-subdirectory config))
(defconst erica-data-directory (erica-user-subdirectory data))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" erica-data-directory))))

(setq load-path (append (list erica-lisp-directory) load-path))
(setq package-user-dir (expand-file-name "packages/" erica-data-directory))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" erica-data-directory))
(setq tramp-persistency-file-name (expand-file-name "tramp" erica-data-directory))
(setq image-dired-dir (expand-file-name "image-dired" erica-data-directory))
(setq transient-history-file (expand-file-name "transient/history.el" erica-data-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" erica-data-directory))
(setq transient-values-file (expand-file-name "transient/values.el" erica-data-directory))

;; Appearance

(setq frame-resize-pixelwise t)
(set-face-attribute 'default (selected-frame) :height 110)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-mode-line '(borderless))
(setq modus-themes-region '(no-extend bg-only accented))

(load-theme 'modus-operandi t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(setq inhibit-startup-screen t)
(setq frame-title-format "Erica")
(setq initial-scratch-message ";; scratch buffer\n\n")

(defalias 'display-startup-echo-area-message 'ignore)

;; Backup

(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Enable Commands

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'suspend-frame 'disabled t)
(put 'suspend-emacs 'disabled t)

;; System

(setq package-enable-at-startup nil)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t))

