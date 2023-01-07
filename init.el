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


;;; Config


(defvar erica-font-mono (font-spec :name "Iberis Mono"))
(defvar erica-font-sans (font-spec :name "Iberis Sans"))
(defvar erica-font-serif  (font-spec :name "Crimson Text"))
(defvar erica-font-mono-serif (font-spec :name "Courier Prime" :weight 'medium))

(defvar erica-font-thai  (font-spec :name "IBM Plex Sans Thai Looped" :weight 'medium))
(defvar erica-font-japanese  (font-spec :name "Source Han Sans JP" :weight 'medium))
(defvar erica-font-chinese  (font-spec :name "Source Han Sans CN" :weight 'medium))
(defvar erica-font-korean  (font-spec :name "Source Han Sans KR" :weight 'medium))
(defvar erica-font-emoji  (font-spec :name "Twitter Color Emoji"))


;;; System

;; enable or disable functions

(mapcar #'(lambda (name) (put name 'disabled nil))
        '(upcase-region downcase-region dired-find-alternate-file narrow-to-region))

(mapcar #'(lambda (name) (put name 'disabled t))
        '(suspend-frame suspend-emacs))

;; packages

(setq custom-file (expand-file-name "custom.el" erica-config-directory))
(when (file-exists-p custom-file) (load custom-file t t))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package package
  :custom
  (package-user-dir (expand-file-name "elpa" erica-data-directory))
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-activate-all))

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

(column-number-mode 1)
(setq frame-resize-pixelwise t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; fonts

(set-fontset-font t nil erica-font-mono)
(set-fontset-font t 'kana erica-font-japanese)
(set-fontset-font t 'hangul erica-font-korean)
(set-fontset-font t 'thai erica-font-thai)
(set-fontset-font t 'han erica-font-japanese)
(set-fontset-font t 'han erica-font-chinese nil 'append)
(set-fontset-font t 'symbol erica-font-emoji)

(set-face-font 'variable-pitch erica-font-sans)
(set-face-font 'fixed-pitch erica-font-mono)
(set-face-font 'fixed-pitch-serif erica-font-mono-serif)

;; theme

(defvar modus-themes-bold-constructs t)
(defvar modus-themes-mode-line '(borderless))
(defvar modus-themes-region '(no-extend bg-only accented))
(defvar modus-themes-links '(no-underline))
(defvar modus-themes-lang-checkers '(background))
(defvar modus-themes-mixed-fonts t)
(load-theme 'modus-operandi t)
(set-face-attribute 'tooltip nil :background (modus-themes-color 'bg-main))

;; whitespaces

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

;; modeline

(use-package diminish)

;; completion

(use-package vertico
  :custom
  ;; TODO: document this
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . cursor-intangible-mode)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode t))

(use-package page-break-lines
  :diminish
  :custom
  (page-break-lines-max-width 110)
  :hook (prog-mode special-mode))

;; compilation

(use-package fancy-compilation
  :custom
  (fancy-compilation-override-colors nil)
  (compile-command (format "%s%s%s" "make -j" (+ 1 (num-processors)) " --no-print-directory -Cbuild"))
  :config
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window)))
  :hook compilation-mode
  :bind ("<f5>" . recompile))

;; help

(setq help-window-select t)
(setq help-window-keep-selected t)

(use-package eldoc
  :diminish)

;; todos

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


;;; Keybinding

(setq use-short-answers t)
(keymap-global-set "<f12>" #'restart-emacs)

;; window

(winner-mode 1)
(windmove-default-keybindings 'meta)

(advice-add 'split-window-below :after (lambda (&rest _) (call-interactively 'other-window)))
(advice-add 'split-window-right :after (lambda (&rest _) (call-interactively 'other-window)))


;;; Editing

(desktop-save-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(c-add-style "c" '("bsd" (c-basic-offset . 4)))
(setq-default fill-column 100)

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "c")))

(setq kill-do-not-save-duplicates t) ; don’t add a string to kill-ring if it duplicates the last one.

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t")
  :config
  (global-hungry-delete-mode 1))

(use-package bookmark+
  :ensure nil
  :init (erica-package-vc-install :repo "emacsmirror/bookmark-plus"))


;;; Major Modes

(use-package geiser
  :defer t)

(use-package geiser-racket
  :defer t
  :hook (scribble-mode . geiser-mode))

(use-package geiser-chez
  :defer t)

(use-package scribble-mode
  :defer t)

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar erica-treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/tjdevries/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  (setq treesit-extra-load-path (list (expand-file-name "treesit" erica-data-directory)))
  (setq major-mode-remap-alist '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode)))
  :config

  (defun erica-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))
  (advice-add
   'treesit--install-language-grammar-1
   :around
   (lambda (old-function out-dir &rest arguments)
     (apply old-function (car treesit-extra-load-path) arguments))))



;;; Tools

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; vc

(setq vc-follow-symlinks t)
(setq project-vc-merge-submodules nil)

;; dired

(setq dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
(defalias 'dired-find-file 'dired-find-alternate-file)

;; flymake

(use-package flymake-popon
  :config
  (defalias 'flymake-eldoc-function #'ignore)
  :hook flymake-mode)

;; pdf

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("o" . pdf-outline)))

;; git

(use-package magit
  :defer t)

(use-package magit-todos
  :hook magit-status-mode)


;;; Shell

(defmacro defcommand (name command)
  `(defun ,(intern (concat "eshell/" (symbol-name name))) (&rest args)
     (eshell-command-result (string-join (cons ,command args) " "))))

(defcommand l "ls")
(defcommand la "ls -A")
(defcommand ll "ls -lh")
(defcommand lla "ls -lhA")
(defcommand pls "sudo")

(defun eshell/em (file)
  (find-file file))

(defun eshell/unpack (&optional file &rest args)
  ;; TODO: support listing file in archive
  (if file
      (let ((command (seq-some (lambda (x)
                                 (if (string-match-p (car x) file)
                                     (cadr x)))
                               '((".*\.tar.bz2" "tar xjf")
                                 (".*\.tar.gz" "tar xzf")
                                 (".*\.bz2" "bunzip2")
                                 (".*\.rar" "unrar x -idq")
                                 (".*\.tgz" "tar xzf")
                                 (".*\.gz" "gunzip")
                                 (".*\.tar" "tar xf")
                                 (".*\.tbz2" "tar xjf")
                                 (".*\.zip" "unzip")
                                 (".*\.Z" "uncompress")
                                 (".*" "echo 'Could not unpack the file:'")))))
        (let ((unpack-command (concat command " " file " " (mapconcat 'identity args " "))))
          (eshell/printnl "Unpack command: " unpack-command)
          (eshell-command-result unpack-command)))
    "unpack: missing file operand"))

(defun erica-shell-prompt ()
  (concat
   (propertize (user-login-name) 'face '(:foreground "#3d5191"))
   (propertize "@" 'face '(:foreground "#3d5191"))
   (propertize (system-name) 'face '(:foreground "#9165ff"))
   " "
   (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "#5d8451"))
   "\n"
   (if (= (user-uid) 0)
       (propertize "# " 'face '(:foreground "#b21818"))
     (propertize "$ " 'face '(:foreground "#5d8451")))))

(defun erica-eshell-clear ()
  (interactive "" eshell-mode)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert "clear 1")
  (eshell-send-input)
  (goto-char (point-max))
  (eshell-bol)
  (yank))

(defun erica-eshell-hook ()
  (setenv "TERM" "eterm-color")
  (local-set-key (kbd "C-c M-o") #'erica-eshell-clear))

(setq eshell-banner-message "")
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq eshell-prompt-regexp "^[$#] ")
(setq eshell-prompt-function #'erica-shell-prompt)

(keymap-global-set "C-c e" #'eshell)

(add-hook 'eshell-mode-hook #'erica-eshell-hook)


;;; End of File
