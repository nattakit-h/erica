;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Nattakit Hosapsin <nattakit@hosapsin.com>
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

(require 'cl-lib)


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

;; packages

(setq package-enable-at-startup nil)
(setq package-user-dir (expand-file-name "elpa" erica-data-directory)) ; required for redirecting gnupg data
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" erica-data-directory))))

(defvar erica-package-host-alist
  '((github . "github.com")
    (gitlab . "gitlab.com")
    (srht . "sr.ht")))

(cl-defun erica-package-vc-install (&key (host 'github) repo name rev backend)
  (let* ((url (format "https://%s/%s" (car (assoc host erica-package-host-alist)) repo))
         (interned-name (when name (intern name)))
         (package-name (or interned-name (intern (file-name-base repo)))))
    (unless (package-installed-p package-name)
      (package-vc-install url interned-name rev backend))))


;;; End of File
