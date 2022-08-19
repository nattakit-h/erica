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



(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-ellipsis "…")
(setq org-catch-invisible-edits 'show-and-error)
(setq org-special-ctrl-a/e t)
(setq org-insert-heading-respect-content t)
(setq org-return-follows-link t)
(setq org-mouse-1-follows-link t)

;; (straight-use-package 'org-roam)
;; (setq org-directory (concat (getenv "HOME") "/documents/notes/"))
;; (setq org-roam-directory org-directory)
;; (setq org-roam-capture-templates
;;       '(("d" "default" plain "%?" :target
;;          (file+head "${slug}.org" "#+title: ${title}")
;;          :unnarrowed t)))
;; (defun erica-roam-node-slug (name)
;;   (apply #'string (mapcar (lambda (c) (if (= c ?_) ?- c)) name)))
;; (advice-add 'org-roam-node-slug :filter-return #'erica-roam-node-slug)
;; (keymap-global-set "C-c n f" #'org-roam-node-find)
;; (keymap-set org-mode-map "C-c n i" #'org-roam-node-insert)
;; (keymap-set org-mode-map "C-c n l" #'org-roam-buffer-toggle)
;; (org-roam-db-autosync-enable)



(provide 'erica-org)
