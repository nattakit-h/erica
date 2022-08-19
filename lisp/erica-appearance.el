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
        (fg-whitespace (modus-themes-color 'fg-whitespace)))
    (set-face-attribute 'whitespace-tab nil :background bg)
    (set-face-attribute 'whitespace-space nil :foreground bg :background bg)
    (set-face-attribute 'whitespace-trailing nil :foreground fg-whitespace :background bg-whitespace)))
(add-hook 'prog-mode-hook  #'whitespace-mode)
(add-hook 'whitespace-mode-hook #'erica-setup-whitespace-faces)



(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))



(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")



(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("NOTE" success bold)
     ("INFO" success bold)
     ("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" error bold)
     ("BUG" error bold)
     ("XXX" error bold)))
  :config
  (global-hl-todo-mode 1))

(use-package ligature
  :straight '(ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures
   't
   '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" ">=" ">>=" "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "<>" ":>"
     ":=" ":-" ":+" "<|" "<|>" "|>" ))
  (global-ligature-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :bind
  (("C-h f" . helpful-callable)
   ("C-h F" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)))


(use-package fancy-compilation
  :straight '(fancy-compilation
              :type git
              :host nil
              :repo "https://codeberg.org/ideasman42/emacs-fancy-compilation.git")
  :custom
  (fancy-compilation-override-colors nil)
  :config
  (fancy-compilation-mode 1))



(provide 'erica-appearance)

