;;; early-init.el --- GNU Emacs early configuration file -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Nattakit Hosapsin <nattakit@hosapsin.com>

;; Author: Nattakit Hosapsin <nattakit@hosapsin.com>
;; Maintainer: Nattakit Hosapsin <nattakit@hosapsin.com>
;; URL: https://github.com/nattakit-h/erica

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Appearance

(modify-all-frames-parameters
 '((right-fringe . 0)
   (left-fringe . 0)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (vertical-scroll-bars . nil)
   (internal-border-width . 0)))

(setq frame-title-format "Erica %& %f")
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; scratch buffer\n\n")
(defalias 'display-startup-echo-area-message #'ignore)

;;; System

;; constants

(defmacro erica-user-subdirectory (path)
  "Return PATH under standard user directory."
  `(expand-file-name ,(symbol-name path) user-emacs-directory))

(defconst erica-config-directory (erica-user-subdirectory config))
(defconst erica-data-directory (erica-user-subdirectory data))
(defconst erica-module-directory (erica-user-subdirectory modules))


;; loading

(add-to-list 'load-path erica-module-directory)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" erica-data-directory))))

(provide 'early-init)

;;; early-init.el ends here

