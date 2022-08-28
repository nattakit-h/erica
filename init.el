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

(defvar erica-input-method-list '("erica" "thai-kesmanee" "japanese-mozc"))


;;; System


;; enable commands

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'suspend-frame 'disabled t)
(put 'suspend-emacs 'disabled t)
(put 'narrow-to-region 'disabled nil)

;; packages

(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (expand-file-name "elpaca/" erica-data-directory))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "builds/elpaca/" elpaca-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca (elpaca :host github :repo "progfolio/elpaca"))

;; files

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(defvar no-littering-var-directory erica-data-directory)
(defvar no-littering-etc-directory erica-config-directory)

(elpaca no-littering
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file) (load-file custom-file))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(global-auto-revert-mode 1)
(global-so-long-mode 1)


;;; Appearance


(column-number-mode 1)
(setq frame-resize-pixelwise t)

(add-hook 'prog-mode-hook (lambda () (setq-local display-line-numbers t)))

;; theme

(defvar modus-themes-bold-constructs t)
(defvar modus-themes-mode-line '(borderless))
(defvar modus-themes-region '(no-extend bg-only accented))
(defvar modus-themes-links '(no-underline))
(defvar modus-themes-lang-checkers '(background))
(defvar modus-themes-mixed-fonts t)
(load-theme 'modus-operandi t)
(set-face-attribute 'tooltip nil :background (modus-themes-color 'bg-main))

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

;; uniquify

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

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
(elpaca hl-todo
  (global-hl-todo-mode 1))

;; ligaures

(elpaca ligature
  (ligature-set-ligatures
   t
   '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
     "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" ">=" ">>="
     "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "<>" ":>"
     ":=" ":-" ":+" "<|" "<|>" "|>" ))
  (global-ligature-mode 1))

;; completion

(defvar savehist-additional-variables '(command-history))
(savehist-mode 1)

(elpaca orderless
  (setq completion-styles '(orderless)))

(elpaca vertico
  (vertico-mode 1)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defvar corfu-auto t)
(defvar corfu-quit-at-boundary t)
(defvar corfu-quit-no-match t)
(elpaca corfu
  (global-corfu-mode t))

(elpaca marginalia
  (marginalia-mode))

(elpaca page-break-lines
  (global-page-break-lines-mode 1))

;; compilation

(defvar compile-command (format "%s%s%s" "make -j" (+ 1 (num-processors)) " --no-print-directory -Cbuild"))
(defvar fancy-compilation-override-colors nil)
(elpaca '(fancy-compilation :repo "https://codeberg.org/ideasman42/emacs-fancy-compilation.git"))
(keymap-global-set "<f5>" #'recompile)
(with-eval-after-load 'compilation
  (advice-add 'compile :after (lambda (&rest _) (call-interactively 'other-window)))
  (advice-add 'recompile :after (lambda (&rest _) (call-interactively 'other-window)))
  (add-hook 'compilation-mode-hook #'fancy-compilation-mode))

;; help

(setq help-window-select t)
(setq help-window-keep-selected t)

(add-to-list 'display-buffer-alist ;; reuse windows
             `(,(rx bos (or "*Apropos*" "*Help*" "*info*" "*Man" "*Shortdoc") (0+ not-newline))
               (display-buffer-reuse-mode-window display-buffer-pop-up-window)
               (mode apropos-mode help-mode Info-mode Man-mode shortdoc-mode)))

(when (executable-find "git")
  (elpaca diff-hl
    (global-diff-hl-mode)
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))))


;;; Keybinding

(setq use-short-answers t)

;; window

(windmove-default-keybindings 'meta)

(advice-add 'split-window-below :after (lambda (&rest _) (call-interactively 'other-window)))
(advice-add 'split-window-right :after (lambda (&rest _) (call-interactively 'other-window)))

;; tabs

(setq tab-bar-show 1)
(keymap-global-set "C-t" nil)
(keymap-global-set "C-t 0" #'tab-close)
(keymap-global-set "C-t 1" #'tab-close-other)
(keymap-global-set "C-t 2" #'tab-new)
(keymap-global-set "C-t <left>" #'tab-previous)
(keymap-global-set "C-t <right>" #'tab-next)
(keymap-global-set "C-t RET" #'tab-switch)

;; search

(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)
(keymap-global-set "C-M-s" #'isearch-forward)
(keymap-global-set "C-M-r" #'isearch-backward)


;;; Editing


(electric-pair-mode 1)
(delete-selection-mode 1)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defvar hungry-delete-chars-to-skip " \t")
(elpaca hungry-delete
  (global-hungry-delete-mode 1))


;;; Major Modes


(elpaca cmake-mode)
(elpaca csharp-mode)
(elpaca glsl-mode)
(elpaca go-mode)
(elpaca yaml-mode)

(defvar font-latex-fontify-sectioning 'color)
(elpaca auctex)

(elpaca scribble-mode)
(elpaca racket-mode
  (add-hook 'racket-before-run-hook #'racket-repl-clear))

(elpaca geiser
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))
  (with-eval-after-load 'geiser
    (defvar geiser-chez-binary
      (cl-some (lambda (name) (executable-find name))
               '("chezscheme" "chez" "chez-scheme")))

    (when geiser-chez-binary
      (elpaca geiser-chez))

    (put 'module 'scheme-indent-function 1)
    (put 'and-let* 'scheme-indent-function 1)
    (put 'parameterize 'scheme-indent-function 1)
    (put 'handle-exceptions 'scheme-indent-function 1)
    (put 'when 'scheme-indent-function 1)
    (put 'unless 'scheme-indent-function 1)
    (put 'match 'scheme-indent-function 1)))

;; eglot


(elpaca (flymake-popon
         :type git
         :repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  (defalias 'flymake-eldoc-function #'ignore)
  (add-hook 'flymake-mode-hook #'flymake-popon-mode))

(defvar eglot-autoshutdown t)
(defvar eglot-send-changes-idle-time 0.25)
(elpaca eglot
  (keymap-global-set "<f8>" #'eglot)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
    (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
    (add-hook 'before-save-hook
              (lambda ()
                (when (eglot-managed-p)
                  (call-interactively #'eglot-format-buffer))))))


;;; Tools

(setq vc-follow-symlinks t)
(setq project-vc-merge-submodules nil)

(setq dired-listing-switches "-laGh1v --group-directories-first --time-style=long-iso")
(defalias 'dired-find-file 'dired-find-alternate-file)

(when (executable-find "git")
  (defvar magit-auto-revert-mode nil)
  (elpaca magit)
  (elpaca magit-todos
    (add-hook 'magit-status-mode-hook #'magit-todos-mode)))

(when (executable-find "rg")
  (elpaca rg)
  (keymap-global-set "C-x p g" #'rg-project)
  (with-eval-after-load 'rg
    (add-to-list 'rg-custom-type-aliases '("el" . "*.el"))
    (add-to-list 'rg-custom-type-aliases '("ss" . "*.ss *.scm *.sls *.sld"))))

;; documents viewers

(elpaca pdf-tools
  (autoload 'pdf-view-mode "pdf-tools")
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'straight-use-package-post-build-functions
            (lambda (name)
              (when (string= "pdf-tools" name)
                (pdf-tools-install t t)))))


(defvar visual-fill-column-center-text t)
(elpaca visual-fill-column)


(defvar nov-text-width 120)
(defvar nov-save-place-file (expand-file-name "nov-places" erica-data-directory))
(elpaca nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun erica-setup-nov-mode ()
    (setq-local fill-column 140)
    (setq-local cursor-type nil)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (face-remap-add-relative 'variable-pitch :family (car erica-font-serif)))

  (add-hook 'nov-mode-hook #'erica-setup-nov-mode))


;;; Shell

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


;;; Language environment


(set-language-environment "UTF-8")

;; input method

(require 'mozc)

(quail-define-package
 "erica" ; NAME
 "UTF-8" ; LANGUAGE
 "(ε)"   ; TITLE
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
 ("\\ga" ?α)    ; GREEK SMALL LETTER ALPHA
 ("\\gb" ?β)    ; GREEK SMALL LETTER BETA
 ("\\gg" ?γ)    ; GREEK SMALL LETTER GAMMA
 ("\\gd" ?δ)    ; GREEK SMALL LETTER DELTA
 ("\\ge" ?ε)    ; GREEK SMALL LETTER EPSILON
 ("\\gz" ?ζ)    ; GREEK SMALL LETTER ZETA
 ("\\gy" ?η)    ; GREEK SMALL LETTER ETA
 ("\\gh" ?θ)    ; GREEK SMALL LETTER THETA
 ("\\gi" ?ι)    ; GREEK SMALL LETTER IOTA
 ("\\gk" ?κ)    ; GREEK SMALL LETTER KAPPA
 ("\\gl" ?λ)    ; GREEK SMALL LETTER LAMDA
 ("\\gm" ?μ)    ; GREEK SMALL LETTER MU
 ("\\gn" ?ν)    ; GREEK SMALL LETTER NU
 ("\\gc" ?ξ)    ; GREEK SMALL LETTER XI
 ("\\go" ?ο)    ; GREEK SMALL LETTER OMNICRON
 ("\\gp" ?π)    ; GREEK SMALL LETTER PI
 ("\\gr" ?ρ)    ; GREEK SMALL LETTER RHO
 ("\\gs" ?σ)    ; GREEK SMALL LETTER SIGMA
 ("\\gt" ?τ)    ; GREEK SMALL LETTER TAU
 ("\\gu" ?υ)    ; GREEK SMALL LETTER UPSILON
 ("\\gf" ?φ)    ; GREEK SMALL LETTER PHI
 ("\\gx" ?χ)    ; GREEK SMALL LETTER CHI
 ("\\gq" ?ψ)    ; GREEK SMALL LETTER PSI
 ("\\gw" ?ω)    ; GREEK SMALL LETTER OMEGA

 ("\\gA" ?Α)    ; GREEK CAPITAL LETTER ALPHA
 ("\\gB" ?Β)    ; GREEK CAPITAL LETTER BETA
 ("\\gG" ?Γ)    ; GREEK CAPITAL LETTER GAMMA
 ("\\gD" ?Δ)    ; GREEK CAPITAL LETTER DELTA
 ("\\gE" ?Ε)    ; GREEK CAPITAL LETTER EPSILON
 ("\\gZ" ?Ζ)    ; GREEK CAPITAL LETTER ZETA
 ("\\gY" ?Η)    ; GREEK CAPITAL LETTER ETA
 ("\\gH" ?Θ)    ; GREEK CAPITAL LETTER THETA
 ("\\gI" ?Ι)    ; GREEK CAPITAL LETTER IOTA
 ("\\gK" ?Κ)    ; GREEK CAPITAL LETTER KAPPA
 ("\\gL" ?Λ)    ; GREEK CAPITAL LETTER LAMDA
 ("\\gM" ?Μ)    ; GREEK CAPITAL LETTER MU
 ("\\gN" ?Ν)    ; GREEK CAPITAL LETTER NU
 ("\\gC" ?Ξ)    ; GREEK CAPITAL LETTER XI
 ("\\gO" ?Ο)    ; GREEK CAPITAL LETTER OMNICRON
 ("\\gP" ?Π)    ; GREEK CAPITAL LETTER PI
 ("\\gR" ?Ρ)    ; GREEK CAPITAL LETTER RHO
 ("\\gS" ?Σ)    ; GREEK CAPITAL LETTER SIGMA
 ("\\gT" ?Τ)    ; GREEK CAPITAL LETTER TAU
 ("\\gU" ?Υ)    ; GREEK CAPITAL LETTER UPSILON
 ("\\gF" ?Φ)    ; GREEK CAPITAL LETTER PHI
 ("\\gX" ?Χ)    ; GREEK CAPITAL LETTER CHI
 ("\\gQ" ?Ψ)    ; GREEK CAPITAL LETTER PSI
 ("\\gW" ?Ω)    ; GREEK CAPITAL LETTER OMEGA

 ("\\^0" ?⁰)    ; SUPERSCRIPT DIGIT ZERO
 ("\\^1" ?¹)    ; SUPERSCRIPT DIGIT ONE
 ("\\^2" ?²)    ; SUPERSCRIPT DIGIT TWO
 ("\\^3" ?³)    ; SUPERSCRIPT DIGIT THREE
 ("\\^4" ?⁴)    ; SUPERSCRIPT DIGIT FOUR
 ("\\^5" ?⁵)    ; SUPERSCRIPT DIGIT FIVE
 ("\\^6" ?⁶)    ; SUPERSCRIPT DIGIT SIX
 ("\\^7" ?⁷)    ; SUPERSCRIPT DIGIT SEVEN
 ("\\^8" ?⁸)    ; SUPERSCRIPT DIGIT EIGHT
 ("\\^9" ?⁹)    ; SUPERSCRIPT DIGIT NINE
 ("\\^+" ?⁺)    ; SUPERSCRIPT PLUS SIGN
 ("\\^-" ?⁻)    ; SUPERSCRIPT MINUS
 ("\\^=" ?⁼)    ; SUPERSCRIPT EQUALS SIGN
 ("\\^(" ?⁽)    ; SUPERSCRIPT LEFT PARENTHESIS
 ("\\^)" ?⁾)    ; SUPERSCRIPT RIGHT PARENTHESIS
 ("\\^i" ?ⁱ)    ; SUPERSCRIPT LATIN SMALL LETTER I
 ("\\^n" ?ⁿ)    ; SUPERSCRIPT LATIN SMALL LETTER N

 ("\\_0" ?₀)    ; SUBSCRIPT ZERO
 ("\\_1" ?₁)    ; SUBSCRIPT ONE
 ("\\_2" ?₂)    ; SUBSCRIPT TWO
 ("\\_3" ?₃)    ; SUBSCRIPT THREE
 ("\\_4" ?₄)    ; SUBSCRIPT FOUR
 ("\\_5" ?₅)    ; SUBSCRIPT FIVE
 ("\\_6" ?₆)    ; SUBSCRIPT SIX
 ("\\_7" ?₇)    ; SUBSCRIPT SEVEN
 ("\\_8" ?₈)    ; SUBSCRIPT EIGHT
 ("\\_9" ?₉)    ; SUBSCRIPT NINE
 ("\\_+" ?₊)    ; SUBSCRIPT PLUS SIGN
 ("\\_-" ?₋)    ; SUBSCRIPT MINUS
 ("\\_=" ?₌)    ; SUBSCRIPT EQUALS SIGN
 ("\\_(" ?₍)    ; SUBSCRIPT LEFT PARENTHESIS
 ("\\_)" ?₎)    ; SUBSCRIPT RIGHT PARENTHESIS

 ("\\ma" ?∀)    ; FOR ALL
 ("\\mu" ?∪)    ; UNION
 ("\\mi" ?∩)    ; INTERSECTION

 ("\\\\" ?\\))  ; REVERSE SOLIDUS

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


;;; End of File
