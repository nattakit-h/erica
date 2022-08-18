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

(use-package awesome-tray
  :straight '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :preface
  (defun erica-awesome-tray-module-input-method-info ()
    (format
     "%s"
     (cond
      ((not current-input-method) "EN")
      ((string-match "thai" current-input-method) "TH")
      ((string-match "mozc" current-input-method) "JP")
      ((string= current-input-method "erica") "EC")
      (t "**"))))

  (defun erica-awesome-tray-module-flymake-info ()
    (with-demoted-errors ""
      (if (and (featurep 'flymake) flymake--state)
          (let* ((known (hash-table-keys flymake--state))
                 (running (flymake-running-backends))
                 (disabled (flymake-disabled-backends))
                 (reported (flymake-reporting-backends))
                 (disabledp (and disabled (null running)))
                 (waiting (cl-set-difference running reported)))
            (when-let
                ((flymake-state
                  (cond
                   (waiting "∿")
                   ((null known) "⁇")
                   (disabledp "⍉")
                   (t (let ((.error 0)
                            (.warning 0)
                            (.note 0))
                        (cl-loop
                         with warning-level = (warning-numeric-level :warning)
                         with note-level = (warning-numeric-level :debug)
                         for state being the hash-values of flymake--state
                         do (cl-loop
                             with diags = (flymake--state-diags state)
                             for diag in diags do
                             (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                            (warning-numeric-level :error))))
                               (cond ((> severity warning-level) (cl-incf .error))
                                     ((> severity note-level)    (cl-incf .warning))
                                     (t                          (cl-incf .note))))))
                        (let ((num (+ .error .warning .note)))
                          (if (> num 0)
                              (string-clean-whitespace
                               (string-join
                                (list
                                 (when (> .note 0)
                                   (propertize (concat "•" (number-to-string .note)) 'face 'awesome-tray-module-flymake-note))
                                 (when (> .warning 0)
                                   (propertize (concat "•" (number-to-string .warning)) 'face 'awesome-tray-module-flymake-warning))
                                 (when (> .error 0)
                                   (propertize (concat "•" (number-to-string .error)) 'face 'awesome-tray-module-flymake-error)))
                                " "))
                            (propertize "•" 'face 'awesome-tray-module-file-path-face))))))))
              flymake-state)))))
  :custom
  (awesome-tray-update-interval 0.1)
  (awesome-tray-info-padding-right 1)
  (awesome-tray-date-format "%y-%m-%d %R")
  (awesome-tray-location-format "%l:%c")
  (awesome-tray-mode-line-active-color "#444444")
  (awesome-tray-active-modules
        '("location"
          "buffer-read-only"
          "buffer-name"
          "flymake*"
          "mode-name"
          "input-method*"))
  (awesome-tray-essential-modules '("buffer-name"))
  (mode-line-format '(mode-line-end-spaces))
  :config
  (add-to-list 'awesome-tray-module-alist
               '("input-method*" . (erica-awesome-tray-module-input-method-info
                                    awesome-tray-module-input-method-face)))
  (add-to-list 'awesome-tray-module-alist
               '("flymake*" . (erica-awesome-tray-module-flymake-info)))
  (awesome-tray-mode 1))


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

