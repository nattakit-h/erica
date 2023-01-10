;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2023 Nattakit Hosapsin <nattakit@hosapsin.com>
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


;;; Appearance

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq frame-title-format "Erica %& %f")
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; scratch buffer\n\n")
(defalias 'display-startup-echo-area-message #'ignore)


;;; System

;; constants

(defmacro erica-user-subdirectory (name)
  `(expand-file-name ,(symbol-name name) user-emacs-directory))
(defconst erica-config-directory (erica-user-subdirectory config))
(defconst erica-data-directory (erica-user-subdirectory data))
(defconst erica-module-directory (erica-user-subdirectory modules))


;; loading

(add-to-list 'load-path erica-module-directory)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" erica-data-directory))))


