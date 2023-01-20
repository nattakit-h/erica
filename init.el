;;; init.el --- GNU Emacs configuration file -*- lexical-binding: t; -*-

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

;;; Constants

(defvar erica-data-directory) ; forward declaration to silence byte-compiled warnings
(defvar erica-config-directory)

(defvar erica-font-mono (font-spec :name "Iberis Mono"))
(defvar erica-font-sans (font-spec :name "Iberis Sans"))
(defvar erica-font-serif  (font-spec :name "Crimson Text"))
(defvar erica-font-mono-serif (font-spec :name "Courier Prime" :weight 'medium))

(defvar erica-font-thai  (font-spec :name "IBM Plex Sans Thai Looped" :weight 'medium))
(defvar erica-font-japanese  (font-spec :name "Source Han Sans JP" :weight 'medium))
(defvar erica-font-chinese  (font-spec :name "Source Han Sans CN" :weight 'medium))
(defvar erica-font-korean  (font-spec :name "Source Han Sans KR" :weight 'medium))

;;; System

;; package manager

(setq package-enable-at-startup nil)
(defvar straight-use-package-by-default t)
(defvar straight-base-dir erica-data-directory)
(defvar straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" erica-data-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; disabled functions

(mapc
 (lambda (name) (put name 'disabled nil))
 '(upcase-region
   downcase-region
   narrow-to-region))

;; files

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(use-package no-littering
  :init
  (defvar no-littering-var-directory erica-data-directory)
  (defvar no-littering-etc-directory erica-config-directory))

(use-package recentf
  :straight nil
  :defer t
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


;;; Appearance

(defun erica-prog-mode-setup ()
  "Display line number and truncate long line in `prog-mode'."
  (let ((inhibit-message t)
        (message-log-max nil))
    (display-line-numbers-mode)
    (toggle-truncate-lines)))
(add-hook 'prog-mode-hook #'erica-prog-mode-setup)

;; fonts

(set-fontset-font t nil erica-font-mono)
(set-fontset-font t 'kana erica-font-japanese)
(set-fontset-font t 'hangul erica-font-korean)
(set-fontset-font t 'thai erica-font-thai)
(set-fontset-font t 'han erica-font-japanese)
(set-fontset-font t 'han erica-font-chinese nil 'append)

(set-face-font 'variable-pitch erica-font-sans)
(set-face-font 'fixed-pitch erica-font-mono)
(set-face-font 'fixed-pitch-serif erica-font-mono-serif)

;; theme

(use-package modus-themes
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (underline-link unspecified)
     (underline-link-visited unspecified)
     (underline-link-symbolic unspecified)))
  :config
  (load-theme 'modus-operandi t))

(use-package marginalia
  :functions (marginalia-mode)
  :config
  (marginalia-mode))

;; whitespaces

(use-package whitespace
  :straight nil
  :after (modus-themes)
  :diminish
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face trailing tabs spaces line tab-mark))
  (whitespace-display-mappings
   '((tab-mark ?\t [?\▷ ?\t])
     (space-mark ?\s [?\·])
     (space-mark ?\xA0 [?\␣])))
  :config
  (defun erica-setup-whitespace-faces ()
    (modus-themes-with-colors
      (set-face-attribute 'whitespace-tab nil :foreground bg-active :background bg-main)
      (set-face-attribute 'whitespace-space nil :foreground bg-main :background bg-main)
      (set-face-attribute 'whitespace-trailing nil :foreground bg-active :background bg-red-nuanced)))
  :hook ((prog-mode . whitespace-mode)
         (whitespace-mode . erica-setup-whitespace-faces)))

;; mode line

(use-package diminish)

;; completion

(use-package vertico
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . cursor-intangible-mode)))

(use-package corfu
  :straight (corfu :files ("*.el" "extensions/*.el"))
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :bind
  ( :map corfu-map
    ("C-d" . corfu-info-documentation)
    ("C-l" . corfu-info-location))
  ;; NOTE: global-corfu-mode doesn't seem to be working when eglot activated
  :hook (prog-mode . corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package kind-icon
  :after (corfu)
  :defines (corfu-margin-formatters)
  :functions (kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; page-break

(use-package page-break-lines
  :diminish
  :custom
  (page-break-lines-max-width 110)
  :hook (prog-mode special-mode))

;; help

(setq help-window-select t)
(setq help-window-keep-selected t)

(use-package eldoc
  :straight nil
  :diminish)

;; flymake

(use-package flymake
  :straight nil
  :after (modus-themes)
  :custom
  (flymake-mode-line-lighter "")
  :config
  (modus-themes-with-colors
    (set-face-attribute 'flymake-note nil :underline nil  :foreground fg-added :background bg-added)
    (set-face-attribute 'flymake-warning nil :underline nil :foreground fg-changed :background bg-changed)
    (set-face-attribute 'flymake-error nil :underline nil  :foreground fg-removed :background bg-removed))
  :hook ((emacs-lisp-mode flymake)))

(use-package popon ; dependency of flymake-popon
  ;; HACK: prevert straight.el from pulling `popone.el' from non existing emacsmirror repo
  :straight (popon :type git :host codeberg :repo "akib/emacs-popon"))

(use-package flymake-popon
  :diminish
  :straight (flymake-popon :type git :host codeberg :repo "akib/emacs-flymake-popon")
  :custom
  ;; HACK: remove default argument which will cause an error because it is a nil indentifier
  (flymake-popon-posframe-extra-arguments '())
  :config
  (defalias 'flymake-eldoc-function #'ignore)
  :hook flymake-mode)

(use-package hl-todo
  :after (modus-themes)
  :defines (hl-todo-highlight-punctuation
            hl-todo-keyword-faces
            ;; defined inside `modus-themes-with-colors'
            cyan-faint
            bg-cyan-nuanced
            yellow-faint
            bg-yellow-nuanced
            red-faint
            bg-red-nuanced)
  :functions (modus-themes-with-colors)
  :config
  (defun erica-hl-todo-faces ()
    (setq hl-todo-highlight-punctuation ":")
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("NOTE" ((t (:weight bold :foreground ,cyan-faint :background ,bg-cyan-nuanced))))
              ("TODO" ((t (:weight bold :foreground ,yellow-faint :background ,bg-yellow-nuanced))))
              ("FIXME" ((t (:weight bold :foreground ,yellow-faint :background ,bg-yellow-nuanced))))
              ("BUG" ((t (:weight bold :foreground ,red-faint :background ,bg-red-nuanced))))
              ("HACK" ((t (:weight bold :foreground ,red-faint :background ,bg-red-nuanced))))
              ("XXX" ((t (:weight bold :foreground ,red-faint :background ,bg-red-nuanced))))))))
  (erica-hl-todo-faces)
  :hook ((modus-themes-after-load-theme . erica-hl-todo-faces)
         (after-init . global-hl-todo-mode)))

(use-package rainbow-mode
  :custom
  (rainbow-x-colors nil))

;; regexp

(use-package anzu
  :functions (global-anzu-mode)
  :diminish
  :config
  (global-anzu-mode 1)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;; compilation

(use-package fancy-compilation
  :defines (compilation-directory)
  :functions (recompile
              project-root
              erica-compile-setup)
  :custom
  (fancy-compilation-override-colors nil)
  (compile-command (format "%s%s%s" "make -j" (+ 1 (num-processors)) " --no-print-directory -Cbuild"))
  :config

  (defun erica-compile-setup (&rest _)
    "Set `compilation-directory' to `project-root' if it's value is nil"
    (unless compilation-directory
      (setq-local compilation-directory (project-root (project-current)))))

  (defun erica-compile-project ()
    "Use `recompile' instead because we can set `compilation-directory' here."
    (interactive)
    (recompile t))

  (advice-add 'recompile :before #'erica-compile-setup)
  (advice-add 'recompile :after (lambda (&rest _) (balance-windows) (other-window 1)))

  :bind (("C-c c"   . erica-compile-project)
         ("C-c C-c" . recompile))

  :hook ((compilation-mode . fancy-compilation-mode)
         (fancy-compilation-setup . (lambda () (setq-local compilation-scroll-output 'first-error)))))

;; search

(use-package ctrlf
  :functions (ctrlf-mode)
  :config
  (ctrlf-mode 1))

;; tabs

(setq tab-bar-show 1) ; only show tab bar when at least 1 tab exist.


;;; Keybinding

(setq use-short-answers t)
(setq confirm-kill-processes nil)
(keymap-global-set "<f12>" #'restart-emacs)
(keymap-global-set "C-z" #'ignore)
(keymap-global-set "C-x C-z" #'ignore)
(keymap-global-set "C-x C-b" #'ibuffer)

;; window

(windmove-mode 1)
(windmove-default-keybindings 'meta)

(advice-add 'split-window-below :after (lambda (&rest _) (balance-windows) (other-window 1)))
(advice-add 'split-window-right :after (lambda (&rest _) (balance-windows) (other-window 1)))

(use-package erica-find-file
  :straight nil
  :custom
  (large-file-warning-threshold 100000000) ; 100 MB
  (find-file-suppress-same-file-warnings nil))


;;; Editing

(delete-selection-mode 1)

(c-add-style "c" '("bsd" (c-basic-offset . 4)))

(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)

(defvar js-indent-level 2)
(defvar css-indent-offset 2)
(defvar c-ts-mode-indent-offset 4)
(defvar c-ts-mode-indent-style 'bsd)
(defvar c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "c")))
(setq kill-do-not-save-duplicates t) ; don’t add a string to kill-ring if it duplicates the last one.

(set-terminal-coding-system 'utf-8)

(use-package electric
  :straight nil
  :config
  (electric-pair-mode 1)
  (add-to-list 'electric-pair-pairs '(?\{ . ?\})))

(use-package hungry-delete
  :diminish
  :functions (global-hungry-delete-mode)
  :custom
  (hungry-delete-chars-to-skip " \t")
  :config
  (global-hungry-delete-mode 1))

(use-package avy
  :bind (("<C-hiragana-katakana>" . avy-goto-line)
         ("<hiragana-katakana>" . avy-goto-char-timer)))

(use-package apheleia
  :diminish
  :functions (apheleia-global-mode)
  :config
  (apheleia-global-mode 1))

(use-package zzz-to-char
  :custom
  (zzz-to-char-reach 2000)
  :bind
  ([remap zap-to-char] . zzz-to-char))

(use-package erica-input
  :straight nil
  :config
  (defun erica-input-setup ()
    (set-language-environment "UTF-8")
    (activate-input-method "erica"))
  :hook (prog-mode . erica-input-setup))

(use-package abbrev
  :straight nil
  :diminish)

(use-package erica-lispy
  :straight nil)


;;; Major modes

(use-package erica-treesit
  :straight nil)

(use-package geiser)
(use-package geiser-chez)
(use-package geiser-racket)

(use-package scribble-mode)

(use-package eglot
  :straight nil
  :mode ("\\.h\\'" . c-ts-mode)
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.25)
  :config
  (let ((ccls-args '((cache . (directory "/tmp/ccls-cache"))
                     (compilationDatabaseDirectory . "build")
                     (completion . (filterAndSort :false)))))
    (add-to-list 'eglot-server-programs
                 `((c-ts-mode c++-ts-mode) . ("ccls" "--init" ,(json-serialize ccls-args)))))
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure))

(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :hook (eglot-managed-mode . eldoc-box-hover-mode))


;;; Tools

;; vc

(setq vc-follow-symlinks t)

(use-package project
  :straight nil
  :custom
  (project-vc-merge-submodules nil))

(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
  (dired-kill-when-opening-new-dired-buffer t))

;; git

(use-package magit
  :defer t)

(use-package magit-todos
  :hook magit-status-mode)

;; pdf

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defines (pdf-view-mode-map)
  :bind (:map pdf-view-mode-map
              ("o" . pdf-outline)))


(use-package erica-shell
  :straight nil)

;; TODO: epub reader: `nov.el'
;; TODO: note taking: `org-roam.el'
;; TODO: session management: `bookmark+.el' and/or `perject.el'
;; TODO: better C-v, M-v https://with-emacs.com/posts/ui-hacks/keep-scrollin-scrollin-scrollin
;;       alternatively: `golden-ration-scroll-mmode.el'
;; TODO: group buffers: `bufler.el'
;; TODO: spell checker: `flyspell' and `ace-popup-menu'



(provide 'init)

;;; init.el ends here

