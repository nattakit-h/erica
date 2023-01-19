;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Nattakit Hosapsin <nattakit@hosapsin.com>
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

(defun eshell/less (file)
  (view-file file))

(defun eshell/unpack (&optional file &rest args)
  ;; TODO: support listing files in the archive
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
   (propertize "$ " 'face '(:foreground "#5d8451"))))

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

(provide 'erica-shell)

;;; End of File
