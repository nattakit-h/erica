;;; erica-disassemble.el --- C Disassemble tools -*- lexical-binding: t; -*-

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

(require 'project)
(require 'json)

(defgroup erica-disassemble nil
  "C disassemble tools."
  :prefix "erica-disassemble-"
  :group 'tools)

(defcustom erica-disassemble-major-mode 'asm-mode
  "Major mode of the disassembled buffer."
  :group 'erica-disassemble
  :type 'symbol)

(defcustom erica-disassemble-program "objdump"
  "Program to disassemble object file."
  :group 'erica-disassemble
  :type 'file)

(defcustom erica-disassemble-objdump-args '("-d" "-M att" "-S" "-l" "--no-addresses" "--no-show-raw-insn")
  "Command-line options for disassemble program."
  :group 'erica-disassemble
  :type '(repeat string))


(defcustom erica-disassemble-find-database-function (lambda () "compile_commands.json")
  "A function to find the project's compilation database.
It will be called without any arguments and should return the path of the
project's database.  If the path is relative, it will be appended to the
root of the project."
  :group 'erica-disassemble
  :type 'function)

(defun erica-disassemble--loopup-database (database file)
  "Lookup DATABASE and return FILE's data."
  (seq-find (lambda (e) (equal (alist-get 'file e nil nil #'eq) file)) database))

;;;###autoload
(defun erica-disassemble-line ()
  "Display the instructions that correspond to the current line of the source file.
This function only supports C projects that have JSON compilation database."
  (interactive)
  (cond
   ;; TODO: handles when database or object file does not exists
   ;; TODO: jump to source line
   ((not (memq major-mode '(c-mode c-ts-mode)))
    (message "Can only be use with C source files"))
   (t
    (if-let ((deasmbuf (get-buffer-create "*erica-disassemble*"))
             (file (buffer-file-name))
             ;; (line (line-number-at-pos nil t))
             (root-path (if (project-current) (project-root (project-current)) default-directory))
             (database-path (funcall erica-disassemble-find-database-function))
             (database-abs-path (if (file-name-absolute-p database-path)
                                    database-path
                                  (expand-file-name database-path root-path)))
             (database (json-read-file database-abs-path))
             (file-database (erica-disassemble--loopup-database database file))
             (binfile (alist-get 'output file-database))
             (cmd (format "%s %s %s"
                          erica-disassemble-program
                          (string-join erica-disassemble-objdump-args " ")
                          binfile)))
        (progn
          (let ((buf (current-buffer))) ; ask to save current buffer
            (save-some-buffers nil (lambda () (equal buf (current-buffer)))))
          (with-current-buffer deasmbuf
            (erase-buffer)
            (shell-command cmd deasmbuf)
            (when (fboundp erica-disassemble-major-mode)
              (funcall erica-disassemble-major-mode))
            (view-buffer deasmbuf #'kill-buffer-if-not-modified)))
      (message "Cannot find current source and object file in the compilation database")))))

(provide 'erica-disassemble)

;;; erica-disassemble.el ends here


