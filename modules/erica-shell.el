;;; erica-shell.el --- Customized eshell with extra aliases -*- lexical-binding: t; -*-

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

(require 'esh-var)
(require 'esh-mode)
(require 'em-basic)
(require 'em-banner)
(require 'em-prompt)

(defmacro erica-shell-defalias (name command)
  "Define a new eshell alias NAME to COMMAND."
  `(defun ,(intern (concat "eshell/" (symbol-name name))) (&rest args)
     (eshell-command-result (string-join (cons ,command args) " "))))

(erica-shell-defalias l "ls")
(erica-shell-defalias la "ls -A")
(erica-shell-defalias ll "ls -lh")
(erica-shell-defalias lla "ls -lhA")
(erica-shell-defalias pls "sudo")

(defun eshell/em (file)
  "Edit FILE in Emacs."
  (find-file file))

(defun eshell/less (file)
  "View FILE in Emacs."
  (view-file file))

(defun eshell/unpack (&optional archive &rest args)
  "Extract various types of ARCHIVE with optional ARGS appended to the command."
  ;; TODO: support listing files in the archive
  (if archive
      (let ((command (seq-some (lambda (x)
                                 (if (string-match-p (car x) archive)
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
        (let ((unpack-command (concat command " " archive " " (mapconcat 'identity args " "))))
          (eshell/printnl "Unpack command: " unpack-command)
          (eshell-command-result unpack-command)))
    "unpack: missing file operand"))

(defun erica-shell-prompt ()
  "Make Prompt string."
  (concat
   (propertize (user-login-name) 'face '(:foreground "#3d5191"))
   (propertize "@" 'face '(:foreground "#3d5191"))
   (propertize (system-name) 'face '(:foreground "#9165ff"))
   " "
   (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "#5d8451"))
   "\n"
   (propertize "$ " 'face '(:foreground "#5d8451"))))

(defun erica-shell-clear ()
  "Clear eshell history."
  (interactive "" eshell-mode)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert "clear 1")
  (eshell-send-input)
  (goto-char (point-max))
  (eshell-bol)
  (yank))

(defun erica-shell-setup ()
  "Setup eshell environment."
  (setenv "TERM" "eterm-color")
  (local-set-key (kbd "C-c M-o") #'erica-shell-clear))

(setq eshell-banner-message "")
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq eshell-prompt-regexp "^[$#] ")
(setq eshell-prompt-function #'erica-shell-prompt)

(keymap-global-set "C-c e" #'eshell)

(add-hook 'eshell-mode-hook #'erica-shell-setup)

(provide 'erica-shell)

;;; erica-shell.el ends here

