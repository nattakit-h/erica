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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erica-font-mono '("Iberis Mono"))
(defvar erica-font-sans '("Iberis Sans"))
(defvar erica-font-serif  '("IBM Plex Serif" :weight medium))
(defvar erica-font-thai  '("IBM Plex Sans Thai Looped" :weight medium))
(defvar erica-font-japanese  '("IBM Plex Sans JP" :weight medium))

(defvar erica-input-method-list '("erica" "thai-kesmanee" "japanese-mozc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enable commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'suspend-frame 'disabled t)
(put 'suspend-emacs 'disabled t)
(put 'narrow-to-region 'disabled nil)

;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq straight-base-dir erica-data-directory)
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(defvar no-littering-var-directory erica-data-directory)
(defvar no-littering-etc-directory erica-config-directory)
(straight-use-package 'no-littering)
(require 'no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(when (file-exists-p custom-file)
  (load-file custom-file))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(global-auto-revert-mode 1)
(global-so-long-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-resize-pixelwise t)

;; theme

(defvar modus-themes-italic-constructs t)
(defvar modus-themes-bold-constructs t)
(defvar modus-themes-mode-line '(borderless))
(defvar modus-themes-region '(no-extend bg-only accented))
(defvar modus-themes-links '(no-underline))
(defvar modus-themes-lang-checkers '(background))
(defvar modus-themes-mixed-fonts t)
(load-theme 'modus-operandi t)
(set-face-attribute 'tooltip nil :background (modus-themes-color 'bg-main))

;; fonts

(set-fontset-font "fontset-default" 'ascii (apply #'font-spec :name erica-font-mono))
(dolist (charset '(kana han cjk-misc)) (set-fontset-font "fontset-default" charset (apply #'font-spec :name erica-font-japanese)))
(set-fontset-font "fontset-default" 'thai (apply #'font-spec :name erica-font-thai))

(apply #'set-face-attribute 'variable-pitch nil :family erica-font-sans)

;; todos

(defvar hl-todo-highlight-punctuation ":")
(defvar hl-todo-keyword-faces
  '(("NOTE" success bold)
    ("INFO" success bold)
    ("TODO" warning bold)
    ("FIXME" error bold)
    ("HACK" error bold)
    ("BUG" error bold)
    ("XXX" error bold)))
(straight-use-package 'hl-todo)
(global-hl-todo-mode 1)

;; ligaures

(straight-use-package 'ligature)
(ligature-set-ligatures
 t
 '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
   "<->" "<-->" "<--->" "<---->" "<!--"
   "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" ">=" ">>="
   "<=>" "<==>" "<===>" "<====>" "<!---"
    "<~~" "<~" "~>" "~~>" "::" ":::" "<>" ":>"
    ":=" ":-" ":+" "<|" "<|>" "|>" ))
(global-ligature-mode 1)

;;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar savehist-additional-variables '(command-history))
(savehist-mode 1)

(straight-use-package 'orderless)
(setq completion-styles '(orderless))

(straight-use-package 'vertico)
(vertico-mode 1)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defvar corfu-auto t)
(defvar corfu-quit-at-boundary t)
(defvar corfu-quit-no-match t)
(straight-use-package 'corfu)

;;; Compilation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar compile-command (format "%s%s%s" "make -j" (+ 1 (num-processors)) " --no-print-directory -Cbuild"))
(defvar fancy-compilation-override-colors nil)
(straight-use-package
 '(fancy-compilation :type git :host nil :repo "https://codeberg.org/ideasman42/emacs-fancy-compilation.git"))
(keymap-global-set "<f5>" #'recompile)
(with-eval-after-load 'compilation
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window)))
  (add-hook 'compilation-mode-hook #'fancy-compilation-mode))

;;; Help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq help-window-select t)
(setq help-window-keep-selected t)

(add-to-list 'display-buffer-alist ;; reuse windows
             `(,(rx bos (or "*Apropos*" "*Help*" "*info*" "*Man" "*Shortdoc") (0+ not-newline))
               (display-buffer-reuse-mode-window display-buffer-pop-up-window)
               (mode apropos-mode help-mode Info-mode Man-mode shortdoc-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-short-answers t)

;;; Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (windmove-default-keybindings)
(advice-add 'split-window-below :after (lambda (&rest _) (call-interactively 'other-window)))
(advice-add 'split-window-right :after (lambda (&rest _) (call-interactively 'other-window)))

;;; Tabs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-bar-show 1)
(keymap-global-set "C-t" nil)
(keymap-global-set "C-t 0" #'tab-close)
(keymap-global-set "C-t 1" #'tab-close-other)
(keymap-global-set "C-t 2" #'tab-new)
(keymap-global-set "C-t <left>" #'tab-previous)
(keymap-global-set "C-t <right>" #'tab-next)
(keymap-global-set "C-t RET" #'tab-switch)

;;; Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)
(keymap-global-set "C-M-s" #'isearch-forward)
(keymap-global-set "C-M-r" #'isearch-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(electric-pair-mode 1)
(delete-selection-mode 1)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defvar hungry-delete-chars-to-skip " \t")
(straight-use-package 'hungry-delete)
(global-hungry-delete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(straight-use-package 'cmake-mode)
(straight-use-package 'csharp-mode)
(straight-use-package 'glsl-mode)
(straight-use-package 'go-mode)
(straight-use-package 'yaml-mode)

(defvar font-latex-fontify-sectioning 'color)
(straight-use-package 'auctex)

(straight-use-package 'scribble-mode)
(straight-use-package 'racket-mode)
(add-hook 'racket-before-run-hook #'racket-repl-clear)

(defvar geiser-chez-binary
  (cl-some (lambda (name) (executable-find name))
           '("chezscheme" "chez" "chez-scheme")))
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))
(with-eval-after-load 'geiser
  (put 'module 'scheme-indent-function 1)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1))

;;; Eglot  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eglot-autoshutdown t)
(defvar eglot-send-changes-idle-time 0.25)
(straight-use-package 'eglot)
(keymap-global-set "<f8>" #'eglot)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (add-hook 'before-save-hook
	    (lambda ()
	      (when (eglot-managed-p)
		(call-interactively #'eglot-format-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq project-vc-merge-submodules nil)

(setq dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
(defalias 'dired-find-file 'dired-find-alternate-file)

(when (executable-find "git")
  (defvar magit-auto-revert-mode nil)
  (straight-use-package 'magit)
  (straight-use-package 'magit-todos)
  (add-hook 'magit-status-mode-hook #'magit-todos-mode))

(when (executable-find "rg")
  (straight-use-package 'rg)
  (keymap-global-set "C-x p g" #'rg-project)
  (with-eval-after-load 'rg
    (add-to-list 'rg-custom-type-aliases '("el" . "*.el"))
    (add-to-list 'rg-custom-type-aliases '("ss" . "*.ss *.scm *.sls *.sld"))))

;;; Documents Viewers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'pdf-tools)
(autoload 'pdf-view-mode "pdf-tools")
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'straight-use-package-post-build-functions
          (lambda (name)
            (when (string= "pdf-tools" name)
              (pdf-tools-install t t))))


(defvar visual-fill-column-center-text t)
(straight-use-package 'visual-fill-column)


(defvar nov-text-width 120)
(defvar nov-save-place-file (expand-file-name "nov-places" erica-data-directory))
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun erica-nov-mode-setup ()
  (setq-local fill-column 140)
  (setq-local cursor-type nil)
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (face-remap-add-relative 'variable-pitch :family (car erica-font-serif)))

(add-hook 'nov-mode-hook #'erica-nov-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun eshell/cat (&optional filename)
  (if filename
      (let ((existing-buffer (get-file-buffer filename))
            (buffer (find-file-noselect filename)))
        (eshell-print
         (with-current-buffer buffer
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (with-no-warnings
               (font-lock-fontify-buffer)))
           (let ((contents (buffer-string)))
             (remove-text-properties 0 (length contents) '(read-only nil) contents)
             contents)))
        (unless existing-buffer
          (kill-buffer buffer))
        nil)
    (eshell-command-result "cat")))

(defun eshell/unpack (&optional file &rest args)
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
        (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
          (eshell/printnl "Unpack command: " unpack-command)
          (eshell-command-result unpack-command)))
    "unpack: missing file operand"))

(defun eshell/em (file)
  (find-file file))

(defun eshell/pls (&rest args)
  (eshell-command-result (string-join (cons "sudo" args) " ")))

(defun eshell/ll (&rest args)
  (eshell-command-result (string-join (cons "ls -lhA" args) " ")))


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

(keymap-global-set "C-c e" #'eshell)

(setq eshell-banner-message "")
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq eshell-prompt-function #'erica-shell-prompt)
(setq eshell-prompt-regexp "^[$#] ")

(add-hook 'eshell-mode-hook #'erica-eshell-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "UTF-8")

;;; Input Method ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mozc)

(quail-define-package
 "erica" ; NAME
 "UTF-8" ; LANGUAGE
 "(ε)"     ; TITLE
 t       ; GUIDANCE
 "Unicode characters input method for scheme programming." ; DOCSTRING
 nil     ; TRANSLATION-KEY
 t       ; FORGET-LAST-SELECTION
 nil     ; DETERMINISTIC
 nil     ; KBD-TRANSLATE
 nil     ; SHOW-LAYOUT
 nil     ; CREATE-DECODE-MAP
 nil     ; MAXIMUM-SHORTEST
 nil     ; OVERLAY-PLIST
 nil     ; UPDATE-TRANSLATION-FUNCION
 nil     ; CONVERSION-KEYS
 t)      ; SIMPLE

(quail-define-rules
 ("\\ga" ?α)	; GREEK SMALL LETTER ALPHA
 ("\\gb" ?β)	; GREEK SMALL LETTER BETA
 ("\\gg" ?γ)	; GREEK SMALL LETTER GAMMA
 ("\\gd" ?δ)	; GREEK SMALL LETTER DELTA
 ("\\ge" ?ε)	; GREEK SMALL LETTER EPSILON
 ("\\gz" ?ζ)	; GREEK SMALL LETTER ZETA
 ("\\gy" ?η)	; GREEK SMALL LETTER ETA
 ("\\gh" ?θ)	; GREEK SMALL LETTER THETA
 ("\\gi" ?ι)	; GREEK SMALL LETTER IOTA
 ("\\gk" ?κ)	; GREEK SMALL LETTER KAPPA
 ("\\gl" ?λ)	; GREEK SMALL LETTER LAMDA
 ("\\gm" ?μ)	; GREEK SMALL LETTER MU
 ("\\gn" ?ν)	; GREEK SMALL LETTER NU
 ("\\gc" ?ξ)	; GREEK SMALL LETTER XI
 ("\\go" ?ο)	; GREEK SMALL LETTER OMNICRON
 ("\\gp" ?π)	; GREEK SMALL LETTER PI
 ("\\gr" ?ρ)	; GREEK SMALL LETTER RHO
 ("\\gs" ?σ)	; GREEK SMALL LETTER SIGMA
 ("\\gt" ?τ)	; GREEK SMALL LETTER TAU
 ("\\gu" ?υ)	; GREEK SMALL LETTER UPSILON
 ("\\gf" ?φ)	; GREEK SMALL LETTER PHI
 ("\\gx" ?χ)	; GREEK SMALL LETTER CHI
 ("\\gq" ?ψ)	; GREEK SMALL LETTER PSI
 ("\\gw" ?ω)	; GREEK SMALL LETTER OMEGA

 ("\\gA" ?Α)	; GREEK CAPITAL LETTER ALPHA
 ("\\gB" ?Β)	; GREEK CAPITAL LETTER BETA
 ("\\gG" ?Γ)	; GREEK CAPITAL LETTER GAMMA
 ("\\gD" ?Δ)	; GREEK CAPITAL LETTER DELTA
 ("\\gE" ?Ε)	; GREEK CAPITAL LETTER EPSILON
 ("\\gZ" ?Ζ)	; GREEK CAPITAL LETTER ZETA
 ("\\gY" ?Η)	; GREEK CAPITAL LETTER ETA
 ("\\gH" ?Θ)	; GREEK CAPITAL LETTER THETA
 ("\\gI" ?Ι)	; GREEK CAPITAL LETTER IOTA
 ("\\gK" ?Κ)	; GREEK CAPITAL LETTER KAPPA
 ("\\gL" ?Λ)	; GREEK CAPITAL LETTER LAMDA
 ("\\gM" ?Μ)	; GREEK CAPITAL LETTER MU
 ("\\gN" ?Ν)	; GREEK CAPITAL LETTER NU
 ("\\gC" ?Ξ)	; GREEK CAPITAL LETTER XI
 ("\\gO" ?Ο)	; GREEK CAPITAL LETTER OMNICRON
 ("\\gP" ?Π)	; GREEK CAPITAL LETTER PI
 ("\\gR" ?Ρ)	; GREEK CAPITAL LETTER RHO
 ("\\gS" ?Σ)	; GREEK CAPITAL LETTER SIGMA
 ("\\gT" ?Τ)	; GREEK CAPITAL LETTER TAU
 ("\\gU" ?Υ)	; GREEK CAPITAL LETTER UPSILON
 ("\\gF" ?Φ)	; GREEK CAPITAL LETTER PHI
 ("\\gX" ?Χ)	; GREEK CAPITAL LETTER CHI
 ("\\gQ" ?Ψ)	; GREEK CAPITAL LETTER PSI
 ("\\gW" ?Ω)	; GREEK CAPITAL LETTER OMEGA

 ("\\^0" ?⁰)	; SUPERSCRIPT DIGIT ZERO
 ("\\^1" ?¹)	; SUPERSCRIPT DIGIT ONE
 ("\\^2" ?²)	; SUPERSCRIPT DIGIT TWO
 ("\\^3" ?³)	; SUPERSCRIPT DIGIT THREE
 ("\\^4" ?⁴)	; SUPERSCRIPT DIGIT FOUR
 ("\\^5" ?⁵)	; SUPERSCRIPT DIGIT FIVE
 ("\\^6" ?⁶)	; SUPERSCRIPT DIGIT SIX
 ("\\^7" ?⁷)	; SUPERSCRIPT DIGIT SEVEN
 ("\\^8" ?⁸)	; SUPERSCRIPT DIGIT EIGHT
 ("\\^9" ?⁹)	; SUPERSCRIPT DIGIT NINE
 ("\\^+" ?⁺)	; SUPERSCRIPT PLUS SIGN
 ("\\^-" ?⁻)	; SUPERSCRIPT MINUS
 ("\\^=" ?⁼)	; SUPERSCRIPT EQUALS SIGN
 ("\\^(" ?⁽)	; SUPERSCRIPT LEFT PARENTHESIS
 ("\\^)" ?⁾)	; SUPERSCRIPT RIGHT PARENTHESIS
 ("\\^i" ?ⁱ)	; SUPERSCRIPT LATIN SMALL LETTER I
 ("\\^n" ?ⁿ)	; SUPERSCRIPT LATIN SMALL LETTER N
 
 ("\\_0" ?₀)	; SUBSCRIPT ZERO
 ("\\_1" ?₁)	; SUBSCRIPT ONE
 ("\\_2" ?₂)	; SUBSCRIPT TWO
 ("\\_3" ?₃)	; SUBSCRIPT THREE
 ("\\_4" ?₄)	; SUBSCRIPT FOUR
 ("\\_5" ?₅)	; SUBSCRIPT FIVE
 ("\\_6" ?₆)	; SUBSCRIPT SIX
 ("\\_7" ?₇)	; SUBSCRIPT SEVEN
 ("\\_8" ?₈)	; SUBSCRIPT EIGHT
 ("\\_9" ?₉)	; SUBSCRIPT NINE
 ("\\_+" ?₊)	; SUBSCRIPT PLUS SIGN
 ("\\_-" ?₋)	; SUBSCRIPT MINUS
 ("\\_=" ?₌)	; SUBSCRIPT EQUALS SIGN
 ("\\_(" ?₍)	; SUBSCRIPT LEFT PARENTHESIS
 ("\\_)" ?₎)	; SUBSCRIPT RIGHT PARENTHESIS

 ("\\ma" ?∀)	; FOR ALL
 ("\\mu" ?∪)	; UNION
 ("\\mi" ?∩)	; INTERSECTION

 ("\\\\" ?\\))	; REVERSE SOLIDUS

(defun erica-cycle-input-method ()
  (interactive)
  (cond
   ((or (not current-input-method)
        (not (member current-input-method erica-input-method-list)))
    (activate-input-method default-input-method))
   ((equal (member current-input-method erica-input-method-list)
           (last erica-input-method-list))
    (activate-input-method (car erica-input-method-list)))
   (t
    (activate-input-method (cadr (member current-input-method erica-input-method-list))))))

(keymap-global-set "C-\\" #'erica-cycle-input-method)


(setq-default default-input-method (car erica-input-method-list))
(activate-input-method (car erica-input-method-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
