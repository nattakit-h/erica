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



(set-fontset-font "fontset-default" 'ascii (font-spec :name "Iberis Mono"))
(dolist (charset '(kana han cjk-misc)) (set-fontset-font "fontset-default" charset (font-spec :name "IBM Plex Sans JP")))
(set-fontset-font "fontset-default" 'thai (font-spec :name "IBM Plex Sans Thai Looped"  :weight 'medium))

(set-face-attribute 'variable-pitch nil :font "Iberis Sans")



(setq show-paren-context-when-offscreen 'overlay)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)



(setq whitespace-line-column 120)
(setq whitespace-style '(face trailing tabs spaces line tab-mark))
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\▷ ?\t])
        (space-mark ?\s [?\·])
        (space-mark ?\xA0 [?\␣])))
(defun erica-setup-whitespace-faces ()
  (let ((bg (modus-themes-color 'bg-main))
        (bg-whitespace (modus-themes-color 'bg-whitespace))
        (fg-alt (modus-themes-color 'fg-alt))
        (fg-whitespace (modus-themes-color 'fg-whitespace)))
    (set-face-attribute 'whitespace-tab nil :background bg)
    (set-face-attribute 'whitespace-space nil :foreground bg :background bg)
    (set-face-attribute 'whitespace-trailing nil :foreground fg-whitespace :background bg-whitespace)))
(add-hook 'prog-mode-hook  #'whitespace-mode)
(add-hook 'whitespace-mode-hook #'erica-setup-whitespace-faces)



(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))
(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))



(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")



(straight-use-package 'marginalia)
(marginalia-mode 1)



(straight-use-package 'page-break-lines)
(global-page-break-lines-mode 1)



(straight-use-package 'simple-modeline)
(simple-modeline-mode 1)



(straight-use-package 'hl-todo)
(setq hl-todo-highlight-punctuation ":")
(setq hl-todo-keyword-faces
      '(("NOTE" success bold)
        ("INFO" success bold)
        ("TODO" warning bold)
        ("FIXME" error bold)
        ("HACK" error bold)
        ("BUG" error bold)
        ("XXX" error bold)))
(global-hl-todo-mode 1)



(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))
(ligature-set-ligatures
 't
 '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
   "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" ">=" ">>=" "<=>" "<==>" "<===>" "<====>" "<!---"
   "<~~" "<~" "~>" "~~>" "::" ":::" "<>" ":>"
   ":=" ":-" ":+" "<|" "<|>" "|>" ))
(global-ligature-mode 1)


(keymap-global-set "<f12>" #'restart-emacs)




(provide 'erica-appearance)

