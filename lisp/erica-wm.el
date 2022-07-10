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



(straight-use-package 'exwm)

(require 'seq)
(require 'subr-x)
(require 'xdg)
(require 'exwm-xim)
(require 'exwm-systemtray)
;; (require 'erica-mozc)

;;;; desktop entry

(defvar erica-wm-desktop-entry-database nil)

(defun erica-wm-desktop-entry-reload ()
  (interactive)
  (setq erica-wm-desktop-entry-database
        (thread-last
          (cons (xdg-data-home) (xdg-data-dirs))
          (mapcar (lambda (path) (expand-file-name (concat path "/applications"))))
          (mapcan (lambda (path) (when (file-directory-p path) (directory-files path t ".+\.desktop"))))
          (mapcar (lambda (file) (xdg-desktop-read-file file)))
          (seq-filter
           (lambda (data)
             (let ((nodisplay (gethash "NoDisplay" data)))
               (and (not (string= nodisplay "true"))
                    (string= (gethash "Type" data) "Application")))))
          (mapcar (lambda (data) (cons (gethash "Name" data) data))))))

;; INFO: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.1.html
;; FIXME: Handle backslashes
(defun erica-wm-desktop-entry--exec-expand (exec)
  (let ((string exec))
    (dolist (var '("%f" "%F" "%u" "%U" "%i" "%c" "%k"))
      (setq string (string-replace var "" string)))
    (string-trim string)))

;; TODO: Add support for marginalia
(defun erica-wm-desktop-entry-launch (command)
  (interactive
   (thread-last
     erica-wm-desktop-entry-database
     ((lambda (apps) (assoc (completing-read "Launch: " apps nil t) apps)))
     (cdr)
     (gethash "Exec")
     (erica-wm-desktop-entry--exec-expand)
     (list)))
  (start-process-shell-command command nil command))

;;;; sendstring

(defvar erica-wm-sendstring-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" #'erica-wm-sendstring-finish)
    (define-key keymap "\C-c\C-q" #'kill-buffer)
    keymap)
  "Keymap for `erica-wm-sendstring-mode-map'")

(define-minor-mode erica-wm-sendstring-mode
  "Minor mode for erica-wm-sendstring"
  :lighter " あ"
  :keymap erica-wm-sendstring-mode-map)

(defun erica-wm-sendstring ()
  "Popup a buffer and let user input, edit and send string to applications."
  (interactive)
  (let ((buffer (get-buffer-create " *erica-sendstring*")))
    (with-current-buffer buffer
      (text-mode)
      (erica-wm-sendstring-mode)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<erica-wm-sendstring-mode-map>"
              "Sendstring: "
              "Finish with `\\[erica-wm-sendstring-finish]', "
              "Ignore with `\\[kill-buffer]'."))))
    (switch-to-buffer buffer)))

(defun erica-wm-sendstring-finish ()
  "Send the content of the current buffer to the kill ring and
then a send paste key sequence to the application to trigger paste operation."
  (interactive nil erica-wm-sendstring-mode)
  (kill-new (buffer-string))
  (kill-buffer)
  (if (derived-mode-p 'exwm-mode)
      (dolist (key (string-to-list (kbd "C-v")))
        (exwm-input--fake-key key))
    (insert string))
  (setq kill-ring (cdr kill-ring)))

(add-to-list 'mini-frame-ignore-commands 'erica-wm-sendstring)

;;;; input

(push ?\C-\\ exwm-input-prefix-keys)

(setq exwm-workspace-number 4)

(setq exwm-input-global-keys
      `(([?\s-l] . erica-wm-desktop-entry-launch)
        ([?\s-i] . erica-wm-sendstring)
        ([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-f] . exwm-floating-toggle-floating)))

(setq exwm-input-simulation-keys
      '(([?\C-p] . [up])
        ([?\C-n] . [down])))

;;;; buffer name

(defun erica-lispcase (str)
  (string-trim-left
   (mapconcat
    (lambda (c)
      (if (string= "Lu" (get-char-code-property c 'general-category))
          (string ?- (downcase c))
        (string c)))
    str)
   "-+"))

(defun erica-wm--exwm-buffer-name ()
  (exwm-workspace-rename-buffer
   (format "<%s>" (erica-lispcase exwm-class-name))))

(add-hook 'exwm-update-class-hook #'erica-wm--exwm-buffer-name)
(add-hook 'exwm-update-title-hook #'erica-wm--exwm-buffer-name)

;;;; appearance

(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(setq display-time-format "%Y-%m-%d %^a %H:%M:%S")

;; TODO Convert to customs
(defvar erica-wm-frame-transparency 85)
(defvar erica-wm-wallpaper-path (expand-file-name "~/pictures/wallpaper.png"))

(defun erica-wm-set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency value: ")
  (assoc-delete-all 'alpha default-frame-alist)
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
  (set-frame-parameter (selected-frame) 'alpha value))

(defun erica-wm-set-wallpaper (path)
  (interactive "fWallpaper path: ")
  (start-process-shell-command "feh" nil (concat "feh --bg-fill --no-fehbg " path)))

;;;; init

(defun erica-wm--init ()
  (advice-add 'ediff-window-display-p :override #'ignore)
  (setenv "XMODIFIERS" "@im=exwm-xim")
  (setenv "GTK_IM_MODULE" "xim")
  (setenv "QT_IM_MODULE" "xim")
  (setenv "PAGER" "cat") 
  (defalias 'undefined #'ignore)
  (exwm-systemtray-enable)
  (exwm-xim-enable)
  (exwm-enable)
  (set-input-method 'erica)
  (display-time-mode t)
  (erica-wm-set-transparency erica-wm-frame-transparency)
  (erica-wm-set-wallpaper erica-wm-wallpaper-path)
  (erica-wm-desktop-entry-reload)
  (global-set-key [?\C-z] nil)
  (global-set-key [?\C-x ?\C-z] nil))

(add-hook 'after-init-hook #'erica-wm--init)



(provide 'erica-wm)

