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



(setq use-short-answers t)
(setq help-window-select t)


(windmove-default-keybindings 'meta)
(keymap-global-set "C-t" nil)
(keymap-global-set "C-t 0" #'tab-close)
(keymap-global-set "C-t 1" #'tab-close-other)
(keymap-global-set "C-t 2" #'tab-new)
(keymap-global-set "C-t <left>" #'tab-previous)
(keymap-global-set "C-t <right>" #'tab-next)
(keymap-global-set "C-t RET" #'tab-switch)
(setq tab-bar-show 1)



(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)
(keymap-global-set "C-M-s" #'isearch-forward)
(keymap-global-set "C-M-r" #'isearch-backward)



(advice-add 'split-window-below :after (lambda (&rest _) (call-interactively 'other-window)))
(advice-add 'split-window-right :after (lambda (&rest _) (call-interactively 'other-window)))



(provide 'erica-binding)
