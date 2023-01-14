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


;;; Config

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
(setq straight-use-package-by-default t)
(setq straight-base-dir erica-data-directory)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

(mapcar
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
  (setq no-littering-var-directory erica-data-directory)
  (setq no-littering-etc-directory erica-config-directory)
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


;;; Appearance

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
  ;; TODO: remap the following faces
  ;; modus-themes-lang-error
  ;; modus-themes-lang-note
  ;; modus-themes-lang-warning
  (load-theme 'modus-operandi t))

(use-package marginalia
  :config
  (marginalia-mode))

;; whitespaces

(use-package whitespace
  :straight nil
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
  ;; NOTE: global-corfu-mode do not work with eglot
  :hook (prog-mode . corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  :custom
  (flymake-mode-line-lighter ""))

(use-package popon ; dependency of flymake-popon
  :straight (popon :type git :host codeberg :repo "akib/emacs-popon"))

(use-package flymake-popon
  :diminish
  :straight 
  (flymake-popon :type git :host codeberg :repo "akib/emacs-flymake-popon")
  :custom
  ;; NOTE: remove default argument which will cause an error because it is a nil indentifier
  (flymake-popon-posframe-extra-arguments '())
  :config
  (defalias 'flymake-eldoc-function #'ignore)
  :hook flymake-mode)

(use-package hl-todo
  :config
  (defun erica-hl-todo-faces ()
    (setq hl-todo-highlight-punctuation ":")
    (setq hl-todo-keyword-faces
          '(("NOTE" . "#0275c2")
            ("INFO" . "#0275c2")
            ("TODO" warning bold)
            ("FIXME" warning bold)
            ("HACK" error bold)
            ("BUG" error bold)
            ("XXX" error bold))))
  (erica-hl-todo-faces)
  :hook ((modus-themes-after-load-theme . erica-hl-todo-faces)
         (after-init . global-hl-todo-mode)))

;; regexp

(use-package anzu
  :diminish
  :config
  (global-anzu-mode 1)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;; compilation

(use-package fancy-compilation
  :custom
  (fancy-compilation-override-colors nil)
  (compile-command (format "%s%s%s" "make -j" (+ 1 (num-processors)) " --no-print-directory -Cbuild"))
  :config
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window)))
  :hook compilation-mode
  :bind ("C-c C-c" . recompile))

;; spelling

(use-package ace-popup-menu
  ;; TODO: integrate with fly-spell
  :custom
  (ace-popup-menu-show-pane-header t)
  :config
  (ace-popup-menu-mode))

;; search

(use-package ctrlf
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

;; window

(windmove-mode 1)
(windmove-default-keybindings 'meta)

(advice-add 'split-window-below :after (lambda (&rest _) (call-interactively 'other-window)))
(advice-add 'split-window-right :after (lambda (&rest _) (call-interactively 'other-window)))


;;; Editing

(electric-pair-mode 1)
(delete-selection-mode 1)
(c-add-style "c" '("bsd" (c-basic-offset . 4)))

(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "c")))
(setq kill-do-not-save-duplicates t) ; don’t add a string to kill-ring if it duplicates the last one.

(use-package hungry-delete
  :diminish hungry-delete-mode
  :custom
  (hungry-delete-chars-to-skip " \t")
  :config
  (global-hungry-delete-mode 1))

(use-package avy
  :bind (("<C-henkan>" . avy-goto-line)
         ("<henkan>" . avy-goto-char-timer)))

(use-package apheleia
  :diminish
  :config
  (apheleia-global-mode 1))

(use-package zzz-to-char
  :custom
  (zzz-to-char-reach 2000)
  :bind
  ([remap zap-to-char] . zzz-to-char))

(require 'erica-lispy)


;;; Major modes

(require 'erica-treesit)

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
                     (compilationDatabaseDirectory . "build"))))
    (add-to-list 'eglot-server-programs
                 `((c-ts-mode c++-ts-mode) . ("ccls" "--init" ,(json-serialize ccls-args)))))
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure))

(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :hook (eglot-managed-mode . eldoc-box-hover-mode))


;;; Tools

;; vc

(setq vc-follow-symlinks t)
(setq project-vc-merge-submodules nil)

;; dired

(setq dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
(setq dired-kill-when-opening-new-dired-buffer t)

;; git

(use-package magit
  :defer t)

(use-package magit-todos
  :hook magit-status-mode)

;; pdf

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("o" . pdf-outline)))

;; TODO: add nov.el
;; TODO: add bookmark+
;; TODO: reimplemnet custom input method

(require 'erica-shell)


;;; End of File
