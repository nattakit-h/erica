;;; erica-lispy --- vi-like Paredit. -*- lexical-binding: t -*-

;; Author: André Peric Tavares <andre.peric.tavares@gmail.com>
;; URL: https://github.com/Andre0991/vilpy
;; Version: 0.1.5 (beta)
;; Keywords: lisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode.

;;; Code:

;;* Requires
; built-in
(eval-when-compile
  (require 'eldoc))
(require 'mode-local)
(require 'help-fns)
(require 'outline)
(require 'newcomment)
(require 'delsel)
(require 'pcase)
(require 'cl-lib)
; external
(require 'avy)


;;; Langugages configuration

(defvar lispy--handlers-alist
  '((:emacs-lisp . ((:decider-fn . (lambda () (or (derived-mode-p 'emacs-lisp-mode)
                                                  (derived-mode-p 'lisp-interaction-mode))))
                    (:eval-last-sexp . eval-last-sexp)
                    (:eval-defun . eval-defun)
                    (:eval-region . eval-region)
		    (:eval-buffer . eval-buffer)
                    (:describe-symbol . lispy--emacs-lisp-describe-symbol)
                    (:indent-sexp . lispy--prettify-emacs-lisp-sexp)))
    (:inf-clojure . ((:decider-fn . (lambda () (bound-and-true-p inf-clojure-minor-mode)))
                     (:eval-last-sexp . inf-clojure-eval-last-sexp)
		     (:describe-symbol . lispy--inf-clojure-describe-symbol)
                     (:eval-defun . inf-clojure-eval-defun)
                     (:eval-region . inf-clojure-eval-region)
		     (:eval-buffer . inf-clojure-eval-buffer)
                     (:indent-sexp . lispy-clojure-indent)))
    (:cider . ((:decider-fn . (lambda () (bound-and-true-p cider-mode)))
               (:eval-last-sexp . cider-eval-last-sexp)
               (:describe-symbol . lispy--cider-describe-symbol)
               (:eval-defun . cider-eval-defun-at-point)
               (:eval-region . cider-eval-region)
	       (:eval-buffer . cider-eval-buffer)
               (:indent-sexp . lispy-clojure-indent)))
    ;; Fallback for clojure, in case `cider` and `inf-clojure` are not activated
    ;; Do not move this up to in this list - that would always ignore cider and inf-clojure,
    ;; which should have higher priority.
    (:clojure . ((:decider-fn . (lambda () (memq major-mode lispy-clojure-modes)))
                 (:indent-sexp . lispy-clojure-indent))))
  "An alist that determine which functions will run for language specific features.
Some commands (eg. `lispy-eval`) consider this list for deciding the appropriate handler
for some feature.
The alist keys are arbitrary, but they tipically represent major or minor modes.
The values are describe below:
`decider-fn`: A function with no arguments that returns non-nil if the set of commands
in this list is appropriate for the current buffer.
`eval-last-sexp`, `eval-defun`, `eval-region`, `describe-symbol` and
`indent-sexp` should be interactive functions.")

(defvar lispy-elisp-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    minibuffer-inactive-mode)
  "Modes for which emacs-lisp related functions are appropriate.")

(defvar lispy-clojure-modes
  '(clojure-mode
    clojurescript-mode
    clojurec-mode)
  "Modes for which clojure related functions are appropriate.")

(defvar lispy-map-input-overlay nil
  "The input overlay for mapping transformations.")

;; TODO: Should this be suspect to comment-char handling as well?
(defvar-local lispy-outline-header ";;"
  "Store the buffer-local outline start.")

;;* Customization
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-left "[([{]"
  "Opening delimiter.")

(defvar lispy-right "[])}]"
  "Closing delimiter.")

(defvar lispy-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defcustom lispy-no-space nil
  "When non-nil, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'lispy)
(make-variable-buffer-local 'lispy-no-space)

(defcustom lispy-lax-eval t
  "When non-nil, fix \"unbound variable\" error by setting the it to nil.
This is useful when hacking functions with &optional arguments.
So evaling (setq mode (or mode major-mode)) will set mode to nil on
the first eval, and to major-mode on the second eval."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-verbose t
  "If t, lispy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-close-quotes-at-end-p nil
  "If t, when pressing the `\"' at the end of a quoted string, it will move you past the end quote."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-avy-style-char 'pre
  "Method of displaying the overlays for a char during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-paren 'at
  "Method of displaying the overlays for a paren during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-symbol 'pre
  "Method of displaying the overlays for a symbol during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-keys (number-sequence ?a ?z)
  "Keys for jumping."
  :type '(repeat :tag "Keys" (character :tag "char")))

(defvar lispy-mode-map (make-sparse-keymap))

(defvar lispy-ignore-whitespace nil
  "When set to t, function `lispy-right' will not clean up whitespace.")

(defcustom lispy-compat '()
  "List of package compatibility options.
Enabling them adds overhead, so make sure that you are actually
using those packages."
  :type '(repeat
          (choice
           (const :tag "god-mode" god-mode)
           (const :tag "magit-blame-mode" magit-blame-mode)
           (const :tag "cider" cider)
           (const :tag "macrostep" macrostep))))

(defvar-local lispy-old-outline-settings nil
  "Store the old values of `outline-regexp' and `outline-level'.
`lispy-mode' overrides those while it's on.")

(defcustom lispy-safe-delete nil
  "When non-nil, killing/deleting an active region keeps delimiters balanced.
This applies to `lispy-delete', `lispy-paste', and
`lispy-delete-backward'."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-copy nil
  "When non-nil, `lispy-copy' won't copy unbalanced delimiters in a region."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-paste nil
  "When non-nil, `lispy-paste' will add missing delimiters."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-threshold 1500
  "The max size of an active region that lispy will try to keep balanced.
This only applies when `lispy-safe-delete', `lispy-safe-copy', and/or
`lispy-safe-paste' are non-nil."
  :group 'lispy
  :type 'number)

(defcustom lispy-safe-actions-ignore-strings t
  "When non-nil, don't try to act safely in strings.
Any unmatched delimiters inside of strings will be copied or deleted. This only
applies when `lispy-safe-delete', `lispy-safe-copy', and/or `lispy-safe-paste'
are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-actions-ignore-comments t
  "When non-nil, don't try to act safely in comments.
Any unmatched delimiters inside of comments will be copied or deleted. This only
applies when `lispy-safe-delete', `lispy-safe-copy', and/or `lispy-safe-paste'
are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-actions-no-pull-delimiters-into-comments nil
  "When non-nil, don't pull unmatched delimiters into comments when deleting.
This prevents the accidental unbalancing of expressions from commenting out
delimiters. This only applies when `lispy-safe-delete', `lispy-safe-copy',
and/or `lispy-safe-paste' are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-insert-space-after-wrap t
  "When non-nil, insert a space after the point when wrapping.
This applies to the commands that use `lispy-pair'."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-thread-last-macro "thread-last"
  "Threading macro to use by default in command `lispy-thread-last'."
  :type '(radio
          (const :tag "Elisp" "thread-last")
          (const :tag "Clojure" "->>")
          (string :tag "Custom")))

(defun lispy-comment-char (&optional level postfix)
  "Get the `comment-start' character, or `;' if nil, repeated LEVEL times concated with POSTFIX."
  (concat
   (apply #'concat (make-list (or level 1) (or comment-start ";")))
   (or postfix "")))

;;;###autoload
(define-minor-mode lispy-mode
  "Minor mode for navigating and editing LISP dialects.

When `lispy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `lispy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{lispy-mode-map}"
  :keymap lispy-mode-map
  :group 'lispy
  :lighter " VP"
  (if lispy-mode
      (progn
        (require 'eldoc)
        (eldoc-remove-command 'special-lispy-eval)
        (eldoc-remove-command 'special-lispy-x)
        (eldoc-add-command 'lispy-space)
        (setq lispy-old-outline-settings
              (cons outline-regexp outline-level))
        (setq-local outline-level 'lispy-outline-level)
        (cond ((eq major-mode 'latex-mode)
               (setq-local lispy-outline "^\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)")
               (setq lispy-outline-header "%")
               (setq-local outline-regexp "\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)"))
              ((eq major-mode 'python-mode)
               (setq-local lispy-outline "^#\\*+")
               (setq lispy-outline-header "#")
               (setq-local outline-regexp "#\\*+")
               (setq-local outline-heading-end-regexp "\n"))
              (t
               (setq-local outline-regexp (substring lispy-outline 1)))))
    (when lispy-old-outline-settings
      (setq outline-regexp (car lispy-old-outline-settings))
      (setq outline-level (cdr lispy-old-outline-settings))
      (setq lispy-old-outline-settings nil))))

;;* Macros
(defmacro lispy-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1)
           (debug (form body)))
  `(let ((i 0))
     (catch 'result
       (condition-case e
           (progn
             (while (<= (cl-incf i) ,n)
               ,@bodyform)
             ,n)
         (error
          (when (eq (car e) 'buffer-read-only)
            (message "Buffer is read-only: %s" (current-buffer)))
          (cl-decf i)
          (and (> i 0) i))))))

(defmacro lispy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  (declare (indent 0))
  `(let ((out (save-excursion
                ,@body)))
     (when (lispy-bolp)
       (back-to-indentation))
     out))

(defmacro lispy-from-left (&rest body)
  "Ensure that BODY is executed from start of list."
  (declare (debug (body)))
  (let ((at-start (cl-gensym "at-start")))
    `(let ((,at-start (lispy--leftp)))
       (unless ,at-start
         (lispy-other))
       (unwind-protect
            (lispy-save-excursion
              ,@body)
         (unless (eq ,at-start (lispy--leftp))
           (lispy-other))))))

(defmacro lispy-flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
            (progn
              (fset ',name (lambda ,@(cdr binding)))
              ,@body)
         (fset ',name ,old)))))

(defmacro lispy-multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

;;* Globals: navigation
(defsubst lispy-right-p ()
  "Return t if after variable `lispy-right'."
  (looking-back lispy-right
                (line-beginning-position)))

(defsubst lispy-left-p ()
  "Return t if before variable `lispy-left'."
  (looking-at lispy-left))

(defsubst lispy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defun lispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call function `lispy-right' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (1+ (cdr bnd)))))
  (let ((pt (point))
        (r (lispy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lispy-right-p))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy--out-forward 1))
      (point))))

(defun lispy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (car bnd))))
  (let ((pt (point))
        (r (lispy-dotimes arg
             (when (= (point) (point-min))
               (error "Reached beginning of buffer"))
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lispy-left-p))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (condition-case nil
              (progn
                (lispy--out-forward 1)
                (backward-list))
            (error
             (progn
               (goto-char pt)
               (up-list -1)))))
      (point))))

(defun lispy-right (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy--remember)
  (when (bound-and-true-p abbrev-mode)
    (ignore-errors (expand-abbrev)))
  (cond ((region-active-p)
         (lispy-mark-right arg))
        ((looking-at lispy-outline)
         (lispy-outline-right))
        (t
         (lispy--out-forward arg))))

(defun lispy-step-out (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (lispy-mark-left arg))
        ((looking-at lispy-outline)
         (lispy-outline-left))
        (t
         (or (lispy--out-backward arg)
             (ignore-errors
               (up-list -1))))))

(defun lispy-out-forward-newline (arg)
  "Call `lispy--out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (lispy--out-forward 1)
  (lispy-dotimes arg
    (newline-and-indent)))

(defun lispy--re-search-in-code (regexp direction &optional count)
  "Move to the next REGEXP in DIRECTION, COUNT times.
DIRECTION is either 'forward or 'backward.
Return the amount of successful moves, or nil otherwise."
  (setq count (or count 1))
  (let ((to-move (abs count))
        (advancer
         (if (eq direction 'forward)
             (if (> count 0)
                 #'re-search-forward
               #'re-search-backward)
           (if (> count 0)
               #'re-search-backward
             #'re-search-forward)))
        (pt (point)))
    (if (and (eq direction 'forward) (> count 0))
        (when (looking-at regexp)
          (goto-char (match-end 0))))
    (while (and (> to-move 0)
                (funcall advancer regexp nil t))
      (unless (lispy--in-string-or-comment-p)
        (cl-decf to-move)))
    (if (= to-move (abs count))
        (progn
          (goto-char pt)
          nil)
      (if (eq direction 'forward)
          (goto-char (match-beginning 0)))
      (- count to-move))))

;;* Locals: navigation
(defun lispy-step-in (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (lispy--remember)
  (let ((pt (point))
        r)
    (cond
      ((and (lispy-bolp)
            (looking-at (lispy-comment-char)))
       (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
      ((lispy-left-p)
       (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
      ((lispy-right-p)
       (backward-char)
       (when (setq r (lispy--re-search-in-code lispy-right 'backward arg))
         (forward-char))))
    (or r
        (progn
          (goto-char pt)
          nil))))

(defun lispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (when leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-forward " \n")
                    (eobp)))
                 ((lispy--symbolp (lispy--string-dwim))
                  (lispy-dotimes arg
                    (when (lispy-slurp 1)
                      (lispy-other)
                      (lispy-barf 1)
                      (lispy-other))))

                 ((looking-at "[\n ]+\\(;\\)")
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lispy--mark (lispy--bounds-comment)))

                 (t
                  (lispy-dotimes arg
                    (forward-sexp 1)
                    (lispy-other)
                    (if (lispy--in-comment-p)
                        (progn
                          (goto-char (1+ (cdr (lispy--bounds-comment))))
                          (skip-chars-forward "\n"))
                      (forward-sexp 2)
                      (forward-sexp -1))
                    (lispy-other))))
           (when leftp
             (exchange-point-and-mark))))

        ((lispy-left-p)
         (lispy-forward arg)
         (let ((pt (point))
               (lispy-ignore-whitespace t))
           (if (lispy-forward 1)
               (lispy-backward 1)
             (goto-char pt)
             (lispy-other))))

        ((lispy-right-p)
         (let ((pt (point)))
           (unless (lispy-forward arg)
             (goto-char pt))))

        ((or (looking-at lispy-outline)
             (and (bolp) (looking-at (lispy-comment-char))))
         (let ((pt (point))
               (outline-regexp lispy-outline))
           (lispy-dotimes arg
             (outline-next-visible-heading 1)
             (if (looking-at lispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "Last outline reached")))))

        (t
         (lispy-forward 1)
         (lispy-backward 1)))
  (lispy--ensure-visible))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (unless leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-backward "\n ")
                    (bobp)))
                 ((looking-back "^ *\\(;\\)[^\n]*[\n ]*"
                                (save-excursion
                                  (ignore-errors
                                    (backward-sexp 1))
                                  (point)))
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lispy--mark (lispy--bounds-comment))
                  (exchange-point-and-mark))
                 ((lispy--symbolp (lispy--string-dwim))
                  (lispy-dotimes arg
                    (when (lispy-slurp 1)
                      (lispy-other)
                      (lispy-barf 1)
                      (lispy-other))))
                 (t
                  (lispy-dotimes arg
                    (backward-sexp 1)
                    (lispy-other)
                    (if (lispy--in-comment-p)
                        (progn
                          (goto-char (1- (car (lispy--bounds-comment))))
                          (skip-chars-backward "\n"))
                      (backward-sexp 2)
                      (backward-sexp -1))
                    (lispy-other))))
           (unless leftp
             (exchange-point-and-mark))))

        ((lispy-left-p)
         (let ((pt (point)))
           (unless (lispy-backward arg)
             (goto-char pt))))

        ((lispy-right-p)
         (lispy-backward arg)
         (let ((pt (point)))
           (if (lispy-backward 1)
               (lispy-forward 1)
             (goto-char pt)
             (lispy-other))))

        ((or (looking-at lispy-outline)
             (and (bolp) (looking-at (lispy-comment-char))))
         (let ((pt (point))
               (outline-regexp lispy-outline))
           (lispy-dotimes arg
             (outline-previous-visible-heading 1)
             (if (looking-at lispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "First outline reached")))))
        (t
         (lispy-backward 1)
         (lispy-forward 1)))
  (lispy--ensure-visible))

(defvar lispy-pos-ring (make-ring 100)
  "Ring for point/mark position and restriction history.")

(defun lispy--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length lispy-pos-ring)))
         (top (unless emptyp
                (ring-ref lispy-pos-ring 0)))
         (restriction (when (buffer-narrowed-p)
                        (cons (set-marker (make-marker)
                                          (point-min))
                              (set-marker (make-marker)
                                          (point-max))))))
    (if (region-active-p)
        (let* ((bnd (lispy--bounds-dwim))
               (bnd (cons
                     (move-marker (make-marker) (car bnd))
                     (move-marker (make-marker) (cdr bnd)))))
          (when (or emptyp
                    (not (equal bnd top)))
            (ring-insert lispy-pos-ring (list bnd restriction))))
      (when (or emptyp
                (not (equal (point-marker) top)))
        (ring-insert lispy-pos-ring (list (point-marker) restriction))))))

(defvar lispy-back-restore-restriction t
  "When non-nil, restore buffer restriction on `lispy-back'.")

(defun lispy-back (arg)
  "Move point to ARGth previous position.
If position isn't special, move to previous or error."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (lispy-dotimes arg
    (if (zerop (ring-length lispy-pos-ring))
        (lispy--complain "At beginning of point history")
      (let* ((data (ring-remove lispy-pos-ring 0))
             (marker (pop data))
             (restriction (pop data))
             (beg (car restriction))
             (end (cdr restriction)))
        ;; After deleting some text, markers that point to it converge
        ;; to one point
        (while (and (not (zerop (ring-length lispy-pos-ring)))
                    (equal (ring-ref lispy-pos-ring 0)
                           marker))
          (ring-remove lispy-pos-ring 0))
        (if (consp marker)
            (lispy--mark marker)
          (deactivate-mark)
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker))
        (when (and lispy-back-restore-restriction
                   restriction)
          (narrow-to-region beg end)
          (set-marker beg nil)
          (set-marker end nil))))))

(defun lispy-knight-down ()
  "Make a knight-like move: down and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-other))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (forward-line)
        (cond ((>= (point) (cdr bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-knight-up ()
  "Make a knight-like move: up and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-other))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (beginning-of-line 0)
        (cond ((< (point) (car bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-other ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((lispy-right-p)
         (backward-list))
        ((lispy-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

(defun lispy-go-to-first-defun ()
  "Sets the mark and moves the point to the first defun."
  (interactive)
  (push-mark)
  (lispy-beginning-of-defun)
  (let ((previous-point (point)))
    (lispy-up 1)
    (while (not (= previous-point (point)))
      (setq previous-point (point))
      (lispy-up 1)))
  (message "Mark saved where command was called"))

(defun lispy-go-to-last-defun ()
  "Sets the mark and moves the point to the last defun."
  (interactive)
  (push-mark)
  (lispy-beginning-of-defun)
  (let ((previous-point (point)))
    (lispy-down 1)
    (while (not (= previous-point (point)))
      (setq previous-point (point))
      (lispy-down 1)))
  (message "Mark saved where command was called"))

;;* Globals: kill, yank, delete, mark, copy
(defun lispy-kill ()
  "Kill line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond ((or (lispy--in-comment-p)
               (and (looking-at " *;")
                    (save-excursion
                      (goto-char (match-end 0))
                      (lispy--in-comment-p))))
           (kill-line))

          ((and (setq bnd (lispy--bounds-string))
                (or
                 (not (eq (point) (car bnd)))
                 (> (count-lines (car bnd) (cdr bnd)) 1)))
           (if (> (cdr bnd) (line-end-position))
               (if (eq (point) (car bnd))
                   (kill-region (car bnd) (cdr bnd))
                 (kill-line))
             (kill-region (point) (1- (cdr bnd)))))
          ((looking-at " *\n")
           (kill-region
            (match-beginning 0)
            (match-end 0))
           (lispy--indent-for-tab))
          ((and (looking-at lispy-right) (looking-back lispy-left
                                                       (line-beginning-position)))
           (delete-char 1)
           (backward-delete-char 1))
          ((lispy-left-p)
           (if (progn
                 (setq bnd (lispy--bounds-list))
                 (> (count-lines (car bnd) (cdr bnd)) 1))
               (kill-region (car bnd)
                            (cdr bnd))
             (narrow-to-region (car bnd) (line-end-position))
             (let ((pt (point)))
               (while (and (ignore-errors
                             (forward-list))
                           (> (point) pt))
                 (setq pt (point)))
               (when (looking-at "[\t ]*;[^\n]*$")
                 (setq pt (match-end 0)))
               (goto-char (point-min))
               (widen)
               (kill-region (point) pt))))
          (t
           (let ((beg (point))
                 (end (line-end-position))
                 bnd)
             (while (and (< (point) end)
                         (ignore-errors
                           (forward-sexp 1)
                           (skip-chars-forward " ")
                           t))
               (when (setq bnd (lispy--bounds-comment))
                 (goto-char (cdr bnd))))
             (skip-chars-forward " \t")
             (kill-region beg (point)))))))

(defun lispy-kill-word (arg)
  "Kill ARG words, keeping parens consistent."
  (interactive "p")
  (if (< arg 0)
      (lispy-backward-kill-word (- arg))
    (let (bnd)
      (lispy-dotimes arg
        (while (not (or (eobp)
                        (memq (char-syntax (char-after))
                              '(?w ?_))))
          (forward-char 1))
        (when (or (lispy-looking-back (concat lispy-left " +"))
                  (lispy-looking-back (lispy-comment-char 1 " +")))
          (delete-horizontal-space))
        (if (setq bnd (lispy--bounds-string))
            (save-restriction
              (narrow-to-region (1+ (car bnd)) (1- (cdr bnd)))
              (kill-word 1)
              (widen))
          (kill-word 1))))))

(defun lispy-backward-kill-word (arg)
  "Kill ARG words backward, keeping parens consistent."
  (interactive "p")
  (let (bnd
        (pt (point))
        (last-command (if (eq last-command 'lispy-backward-kill-word)
                          'kill-region
                        last-command)))
    (lispy-dotimes arg
      (when (lispy--in-comment-p)
        (skip-chars-backward " \n"))
      (if (memq (char-syntax (char-before))
                '(?w ?_ ?\s))
          (if (lispy-looking-back "\\_<\\s_+")
              (delete-region (match-beginning 0)
                             (match-end 0))
            (backward-kill-word 1)
            (when (and (lispy--in-string-p)
                       (not (lispy-looking-back "\\\\\\\\"))
                       (lispy-looking-back "\\\\"))
              (delete-char -1)))
        (delete-region (point) pt)
        (while (not (or (bobp)
                        (memq (char-syntax (char-before))
                              '(?w ?_))))
          (backward-char 1))
        (if (setq bnd (lispy--bounds-string))
            (progn
              (save-restriction
                (if (and (looking-at "\\s-+\"")
                         (eq (match-end 0) (cdr bnd)))
                    (goto-char (1- (cdr bnd)))
                  (when (and (> pt (car bnd))
                             (< pt (cdr bnd)))
                    (goto-char pt)))
                (narrow-to-region (1+ (car bnd)) (point))
                (kill-region (progn
                               (forward-word -1)
                               (when (and (not (lispy-looking-back "\\\\\\\\"))
                                          (lispy-looking-back "\\\\"))
                                 (backward-char))
                               (point))
                             (point-max))
                (widen)))
          (backward-kill-word 1))))))

(defvar lispy-delete-sexp-from-within nil
  "When cursor is adjacent to an opening or closing pair,
`lispy-delete' or `lispy-delete-backward' toward the delimiter
will kill the whole sexp (string or list).")

(defun lispy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lispy-delete-backward (- arg)))

          ((region-active-p)
           (lispy--maybe-safe-delete-region (region-beginning) (region-end)))

          ((setq bnd (lispy--bounds-string))
           (cond ((eq (1+ (point)) (cdr bnd))
                  (goto-char (car bnd))
                  (when lispy-delete-sexp-from-within
                    (lispy-delete arg)))
                 ((looking-at "\\\\\"")
                  (if (eq (+ (point) 2) (cdr bnd))
                      (goto-char (car bnd))
                    (delete-char 2)))
                 ((and (looking-at "\"")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((lispy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
                 ((looking-at "\\\\\\\\")
                  (delete-char 2))
                 ((and (looking-at "\\\\")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((eq (point) (car bnd))
                  (delete-region (car bnd)
                                 (cdr bnd))
                  (let ((pt (point)))
                    (skip-chars-forward " ")
                    (delete-region pt (point))))
                 ((save-excursion
                    (forward-char 1)
                    (lispy--in-string-or-comment-p))
                  (delete-char arg))
                 (t
                  (lispy--exit-string))))

          ((lispy--in-comment-p)
           (if (lispy-bolp)
               (let ((bnd (lispy--bounds-comment)))
                 (delete-region (car bnd) (cdr bnd)))
             (delete-char arg)))

          ((looking-at lispy-right)
           (lispy-step-out 1)
           (when lispy-delete-sexp-from-within
             (lispy-delete arg)))

          ((lispy-left-p)
           (lispy--delete-leading-garbage)
           (lispy-dotimes arg
             (lispy--delete)))

          ((eolp)
           (delete-char 1)
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point))
             (unless (or (eolp)
                         (bolp)
                         (lispy-bolp)
                         (eq (char-before) ?\ ))
               (insert " "))))

          (t
           (delete-char arg)))))

(defun lispy--delete-leading-garbage ()
  "Delete any syntax before an opening delimiter such as '.
Delete backwards to the closest whitespace char or opening delimiter or to the
beginning of the line."
  (let ((pt (point))
        (end (save-excursion (re-search-backward ")" nil t))))
    (re-search-backward
     (concat "[[:space:]]" "\\|" lispy-left "\\|" "^")
     end t)
    (goto-char (match-end 0))
    (delete-region (point) pt)))

(defun lispy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defvar lispy-delete-backward-recenter -20
  "When cursor is near top of screen when calling
  `lispy-delete-backward', recenter cursor with arg.")

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lispy-delete (- arg)))

          ((use-region-p)
           (lispy--maybe-safe-delete-region (region-beginning)
                                            (region-end)))
          ((bobp))

          ((and (setq bnd (lispy--bounds-string))
                (not (eq (point) (car bnd))))
           (cond ((eq (- (point) (car bnd)) 1)
                  (goto-char (cdr bnd))
                  (if lispy-delete-sexp-from-within
                      (lispy-delete-backward arg)))
                 ((or (looking-back "\\\\\\\\(" (car bnd))
                      (looking-back "\\\\\\\\)" (car bnd)))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (lispy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 ((looking-back "[^\\]\\\\[^\\]" (car bnd))
                  (backward-delete-char 2))
                 (t
                  (backward-delete-char-untabify arg))))

          ((looking-at lispy-outline)
           (if (lispy-looking-back (concat lispy-outline ".*\n"))
               (delete-region
                (match-beginning 0)
                (match-end 0))
             (delete-char -1)))

          ((lispy--in-comment-p)
           (cond ((lispy-looking-back "^ +")
                  (delete-region (max (1- (match-beginning 0))
                                      (point-min))
                                 (match-end 0))
                  (lispy--indent-for-tab))
                 ((and (looking-at "$") (lispy-looking-back (lispy-comment-char 1 " +")))
                  (let ((pt (point)))
                    (skip-chars-backward " ;")
                    (delete-region (point) pt)
                    (if (lispy-looking-back "^")
                        (lispy--indent-for-tab)
                      (let ((p (point)))
                        (lispy--out-forward 1)
                        (lispy--prettify-1)
                        (goto-char p)))))
                 (t
                  (backward-delete-char-untabify arg))))

          ((lispy-looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (lispy-looking-back (concat lispy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (and (lispy-right-p)
                    (or (memq major-mode lispy-clojure-modes)
                        (not (lispy-looking-back "[\\?]."))))
               (and (lispy-looking-back (concat lispy-right " "))
                    (or (lispy-left-p) (looking-at "\""))))
           (let ((pt (point)))
             (lispy-backward arg)
             (unless (lispy-right-p)
               (lispy--skip-delimiter-preceding-syntax-backward))
             (skip-chars-backward " \t")
             (while (plist-get (text-properties-at (point)) 'read-only)
               (forward-char))
             (delete-region (point) pt)
             (unless (or (looking-at " ")
                         (lispy-bolp)
                         (and (lispy-right-p)
                              (not (or (lispy-left-p)
                                       (looking-at "\""))))
                         (lispy-looking-back lispy-left)
                         ;; REPL prompt, e.g. `ielm'
                         (lispy-after-string-p "> "))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (lispy-bolp))
                  (not (lispy-left-p))
                  (progn
                    (skip-chars-backward " \t\n")
                    (lispy-right-p)))
                 (delete-region (point) pt)
               (goto-char pt)
               (lispy--indent-for-tab))))

          ((and (lispy-looking-back lispy-left)
                (not (lispy-looking-back "[\\?].")))
           (lispy--out-forward 1)
           (lispy-delete-backward 1))

          ((eq (char-before) ?\")
           (backward-char 1)
           (let ((bnd (lispy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (lispy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (lispy--indent-for-tab)))

          ((and (lispy-after-string-p "\" ")
                (not (looking-at lispy-right)))
           (let ((pt (point)))
             (backward-char 2)
             (delete-region (car (lispy--bounds-string)) pt))
           (lispy--delete-whitespace-backward)
           (unless (lispy-looking-back lispy-left)
             (just-one-space))
           (lispy--indent-for-tab))

          ((lispy-bolp)
           (delete-region
            (line-beginning-position)
            (point))
           (unless (bobp)
             (if (and (not (eolp))
                      (save-excursion
                        (backward-char 1)
                        (lispy--in-comment-p)))
                 (progn
                   (backward-char 1)
                   (let ((bnd (lispy--bounds-comment)))
                     (delete-region (car bnd) (cdr bnd)))
                   (delete-char 1))
               (backward-delete-char 1)
               (unless (or (eolp)
                           (looking-at lispy-right)
                           (lispy-looking-back lispy-left))
                 (just-one-space)))
             (lispy--indent-for-tab)))

          ((lispy-looking-back "[^ ]  +")
           (delete-region (+ (match-beginning 0) 2) (point)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5)
             lispy-delete-backward-recenter)
    (ignore-errors
      (recenter lispy-delete-backward-recenter)))
  (when (and (lispy-left-p)
             (not (lispy--in-string-or-comment-p)))
    (indent-sexp)))

(defun lispy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (lispy--bounds-comment)
                    (lispy--bounds-string)
                    (lispy--bounds-list))))
    (when bounds
      (lispy--mark bounds))))

(defun lispy-mark-list (arg)
  "Mark list from special position.
When ARG is more than 1, mark ARGth element."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (cond ((> arg 1)
         (lispy-mark-car)
         (lispy-down (1- arg)))
        ((= arg 0)
         (let ((bnd (lispy--bounds-dwim)))
           (lispy--mark
            (cons (+ (car bnd) (if (eq (char-after (car bnd)) ?\#) 2 1))
                  (1- (cdr bnd))))))
        ((region-active-p)
         (deactivate-mark)
         (if (lispy--in-comment-p)
             (progn
               (beginning-of-line)
               (skip-chars-forward " "))
           (skip-chars-forward ",@'`")))
        ((lispy-left-p)
         (lispy--mark
          (lispy--bounds-dwim)))
        ((lispy-right-p)
         (lispy--mark
          (lispy--bounds-dwim))
         (lispy-other))
        ((and (lispy-bolp) (looking-at (lispy-comment-char)))
         (lispy--mark (lispy--bounds-comment))))
  (setq this-command 'lispy-mark-list))

(declare-function evil-execute-in-normal-state "ext:evil-execute-in-normal-state")
(defun lispy-execute-in-normal-state ()
  "Execute `evil-execute-in-normal-state` and restore the previous evil state."
  (interactive)
  ;; `evil-execute-in-normal-state` uses `this-command` internally for deciding if the
  ;; previous position will be restored. We need to to set it here, otherwise `this-command`
  ;; would get a `special` prefix and the it wouldn't get back to insert state.
  (setq this-command 'evil-execute-in-normal-state)
  (evil-execute-in-normal-state))

(defvar-local lispy-bind-var-in-progress nil)

(defun lispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (let (bnd)
    (cond (lispy-bind-var-in-progress
           (lispy-map-done)
           (setq lispy-bind-var-in-progress nil)
           (forward-sexp 2)
           (lispy-mark-symbol))

          ((lispy--in-comment-p)
           (if (and (looking-at "\\(?:\\w\\|\\s_\\)*'")
                    (setq bnd (match-end 0))
                    (looking-back "`\\(?:\\w\\|\\s_\\)*"
                                  (line-beginning-position)))
               (progn
                 (goto-char (match-beginning 0))
                 (set-mark (point))
                 (goto-char bnd))
             (lispy--mark (lispy--bounds-comment))))

          ((and
            (not (region-active-p))
            (setq bnd (lispy--bounds-string))
            (= (1+ (point))
               (cdr bnd)))
           (lispy--mark bnd))

          ((and (lispy-after-string-p "\"")
                (not (lispy--in-string-or-comment-p)))
           (set-mark-command nil)
           (forward-sexp -1)
           (exchange-point-and-mark))

          ((looking-at " *[[({]")
           (if (and (lispy-looking-back "\\sw\\|\\s_")
                    (not (region-active-p)))
               (progn
                 (set-mark-command nil)
                 (forward-sexp -1)
                 (exchange-point-and-mark))
             (let ((pt (point)))
               (skip-chars-forward "(){}[] \"\n")
               (set-mark-command nil)
               (if (looking-at "\\sw\\|\\s_")
                   (forward-sexp)
                 (condition-case nil
                     (progn
                       (re-search-forward "[][(){} \n]")
                       (while (lispy--in-string-or-comment-p)
                         (re-search-forward "[() \n]"))
                       (backward-char 1))
                   (error
                    (message "No further symbols found")
                    (deactivate-mark)
                    (goto-char pt)))))))

          ((region-active-p)
           (let ((bnd (lispy--bounds-string)))
             (condition-case nil
                 (progn
                   (forward-sexp)
                   (when (and bnd (> (point) (cdr bnd)))
                     (goto-char (cdr bnd))
                     (error "`forward-sexp' went through string bounds")))
               (error
                (deactivate-mark)
                (re-search-forward "\\sw\\|\\s_")
                (forward-char -1)
                (set-mark-command nil)
                (forward-sexp)))))

          ((lispy-right-p)
           (skip-chars-backward "}]) \n")
           (set-mark-command nil)
           (re-search-backward "[][{}() \n]")
           (while (lispy--in-string-or-comment-p)
             (re-search-backward "[() \n]"))
           (forward-char 1))

          ((looking-at lispy-right)
           (lispy--mark
            (save-excursion
              (backward-char 1)
              (lispy--bounds-dwim))))

          (t
           (lispy--mark (lispy--bounds-dwim))))))

(defun lispy-copy ()
  "Copy marked region or sexp to kill ring."
  (interactive)
  (let ((str (if (region-active-p)
                 (lispy--maybe-safe-region (region-beginning)
                                           (region-end))
               (lispy--string-dwim))))
    (unless (equal str (ignore-errors
                         (current-kill 0)))
      (kill-new str))))

;;* Globals: pairs
(defvar lispy-parens-only-left-in-string-or-comment t
  "Whether \"(\" should insert only the left paren in strings and comments.")

(defun lispy-pair (left right preceding-syntax-alist)
  "Return (lambda (arg)(interactive \"P\")...) using LEFT RIGHT.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede LEFT in each major mode.
When this function is called:
- with region active:
  Wrap region with LEFT RIGHT.
- with region active and arg 1:
  Wrap region with LEFT RIGHT and put the point after LEFT followed by a space.
- with arg nil:
  Insert LEFT RIGHT.
- with arg negative:
  Wrap as many sexps as possible until the end of the line with LEFT RIGHT.
- with arg 0:
  Wrap as many sexps as possible with LEFT RIGHT.
- with the universal arg:
  Wrap one sexp with LEFT RIGHT.
- with arg positive:
  Wrap that number of sexps with LEFT RIGHT or as many as possible."
  `(lambda (arg)
     (interactive "P")
     (cond ((not arg))
           ((listp arg)
            (setq arg 1))
           (t
            (setq arg (prefix-numeric-value arg))))
     (cond ((region-active-p)
            (lispy--surround-region ,left ,right)
            (when (and (lispy-looking-back lispy-left)
                       (or (lispy-left-p)
                           (> (or arg 0) 0)))
              (insert " "))
            (backward-char 1))
           ((and (lispy--in-string-p)
                 (lispy-looking-back "\\\\\\\\"))
            (insert ,left "\\\\" ,right)
            (backward-char 3))
           ((lispy--in-string-or-comment-p)
            (if (and lispy-parens-only-left-in-string-or-comment
                     (string= ,left "(")
                     (= ?\( (aref (this-command-keys-vector) 0)))
                (insert "(")
              (insert ,left ,right)
              (backward-char 1)))
           ((lispy-after-string-p "?\\")
            (insert ,left))
           ((not arg)
            (lispy--indent-for-tab)
            (lispy--delimiter-space-unless ,preceding-syntax-alist)
            (insert ,left ,right)
            (unless (or (eolp)
                        (lispy--in-string-p)
                        (looking-at "\n\\|)\\|}\\|\\]"))
              (just-one-space)
              (backward-char 1))
            (when (looking-at ,(regexp-quote left))
              (insert " ")
              (backward-char))
            (backward-char))
           (t
            ;; don't jump backwards or out of a list when not at a sexp
            (unless (or (lispy--not-at-sexp-p ,preceding-syntax-alist)
                        (and (memq major-mode lispy-clojure-modes)
                             (looking-at lispy-left)
                             (lispy-after-string-p "#")))
              (when (lispy--bounds-dwim)
                (goto-char (car (lispy--bounds-dwim)))))
            (lispy--indent-for-tab)
            (insert ,left ,right)
            (save-excursion
              (lispy-slurp arg))
            (when (or (looking-at lispy-right)
                      (and (eolp)
                           (looking-back lispy-right (1- (point)))))
              ;; failed to wrap anything
              (backward-char))
            (when (and lispy-insert-space-after-wrap
                       (not (lispy--in-empty-list-p ,preceding-syntax-alist))
                       (not (eolp)))
              (just-one-space)
              (backward-char))))))

(defvar lispy-parens-preceding-syntax-alist
  '((lisp-mode . ("[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
    (emacs-lisp-mode . ("[#`',@]+" "#s" "#[0-9]+="))
    (clojure-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurescript-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurec-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-repl-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-clojure-interaction-mode . ("[`'~@]+" "#" "#\\?@?"))
    (janet-mode . ("[@;]"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . ("[`',@]+")))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening paren in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-parens'.")

(defvar lispy-braces-preceding-syntax-alist
  '((clojure-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurescript-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurec-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-repl-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-clojure-interaction-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (janet-mode . ("[@;]"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening brace in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-braces'.")

(defalias 'lispy-parens
    (lispy-pair "(" ")" 'lispy-parens-preceding-syntax-alist)
  "`lispy-pair' with ().")

(defalias 'lispy-braces
    (lispy-pair "{" "}" 'lispy-braces-preceding-syntax-alist)
  "`lispy-pair' with {}.")

(defun lispy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (lispy-unstringify)
             (if (and lispy-close-quotes-at-end-p (looking-at "\""))
                 (forward-char 1)
                 (progn (insert "\\\"\\\""))
               (backward-char 2))))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun lispy-open-line-below ()
  "Insert an empty line below the current line and move the point to it."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-char)
  (lispy--indent-for-tab))

(defun lispy-open-line-above ()
  "Insert parenthesis in an empty line above the current line and move the point to them."
  (interactive)
  (end-of-line 0)
  (open-line 1)
  (forward-char)
  (lispy--indent-for-tab))

(defun lispy-parens-down ()
  "Exit the current sexp, and start a new sexp below."
  (interactive)
  (condition-case nil
      (progn
        (lispy--out-forward 1)
        (if (looking-at "\n *\\()\\)")
            (progn
              (goto-char (match-beginning 1))
              (insert "()")
              (lispy--indent-for-tab)
              (backward-char))

          (insert "\n()")
          (lispy--indent-for-tab)
          (backward-char)))
    (error (indent-new-comment-line))))

;;* Globals: insertion
(defun lispy-space (arg)
  "Insert one space, with position depending on ARG.
If ARG is 2, amend the current list with a space from current side.
If ARG is 3, switch to the different side beforehand.
If jammed between parens, \"(|(\" unjam: \"(| (\". If after an opening delimiter
and before a space (after wrapping a sexp, for example), do the opposite and
delete the extra space, \"(| foo)\" to \"(|foo)\"."
  (interactive "p")
  (cond ((region-active-p)
         (goto-char (region-end))
         (deactivate-mark)
         (insert " "))
        ((lispy--in-string-or-comment-p)
         (call-interactively 'self-insert-command))
        ((eq arg 4)
         (when (lispy--leftp)
           (lispy-other))
         (backward-char)
         (unless (lispy-bolp)
           (newline-and-indent)))
        ((or (eq arg 2)
             (when (eq arg 3)
               (lispy-other)
               t))

         (if (lispy-left-p)
             (progn
               (forward-char)
               (just-one-space)
               (backward-char))
           (backward-char)
           (just-one-space)))
        ((and (lispy-looking-back lispy-left)
              (not (eq ?\\ (char-before (match-beginning 0)))))
         (if (looking-at "[[:space:]]")
             (delete-region (point)
                            (progn
                              (skip-chars-forward "[:space:]")
                              (point)))
           (call-interactively 'self-insert-command)
           (backward-char)))
        (t
         (call-interactively 'self-insert-command)
         (when (and (lispy-left-p)
                    (lispy-looking-back "[[({] "))
           (backward-char)))))

(defvar lispy-brackets-preceding-syntax-alist
  '((clojure-mode . ("[`']" "#[A-z.]*"))
    (clojurescript-mode . ("[`']" "#[A-z.]*"))
    (clojurec-mode . ("[`']" "#[A-z.]*"))
    (cider-repl-mode . ("[`']" "#[A-z.]*"))
    (cider-clojure-interaction-mode . ("[`']" "#[A-z.]*"))
    (janet-mode . ("[@;]"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening bracket in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-brackets'.")

(defalias 'lispy-brackets
    (lispy-pair "[" "]" 'lispy-brackets-preceding-syntax-alist)
    "`lispy-pair' with [].")

(defun lispy-tick (arg)
  "Insert ' ARG times.
When the region is active and marks a string, unquote it.
Otherwise, when the region is active, toggle ' at the start of the region."
  (interactive "p")
  (cond ((lispy--string-markedp)
         (lispy-unstringify))
        ((region-active-p)
         (lispy-toggle-char ?\'))
        (t
         (lispy--space-unless "\\s-\\|\\s(\\|[~#:?'`]\\|\\\\")
         (self-insert-command arg))))

(defun lispy-underscore (&optional arg)
  "Insert _ ARG times.
For Clojure modes, toggle #_ sexp comment."
  (interactive "p")
  (setq arg (or arg 1))
  (if (memq major-mode lispy-clojure-modes)
      (let ((leftp (lispy--leftp)))
        (unless leftp
          (lispy-other))
        (if (lispy-after-string-p "#_")
            (delete-char -2)
          (insert "#_"))
        (unless leftp
          (lispy-other)))
    (self-insert-command arg)))

(defun lispy-backtick ()
  "Insert `.
When the region is active, surrounds it with backticks."
  (interactive)
  (if (region-active-p)
      (lispy--surround-region "`" "'")
    (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
    (insert "`")))

(defun lispy-toggle-char (char)
  "Toggle CHAR at the start of the region."
  (let ((bnd (lispy--bounds-dwim))
        deactivate-mark)
    (save-excursion
      (goto-char (car bnd))
      (if (eq (char-after) char)
          (delete-char 1)
        (insert char)))))

(defun lispy-hash ()
  "Insert #."
  (interactive)
  (if (and (or (memq major-mode lispy-clojure-modes)
               (memq major-mode '(nrepl-repl-mode
                                  cider-clojure-interaction-mode)))
           (lispy-looking-back "\\sw #"))
      (progn
        (backward-delete-char 2)
        (insert "#"))
    (lispy--space-unless "\\s-\\|\\s(\\|[#:?'`,]\\\\?")
    (insert "#")))

(declare-function cider-eval-print-last-sexp "ext:cider-eval")
(declare-function cider-repl-newline-and-indent "ext:cider-repl")
(declare-function ielm-return "ielm")
(declare-function mode-local-bind "mode-local")

(defun lispy-newline-and-indent ()
  "Insert newline."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode)
         (setq this-command 'eval-last-sexp)
         (eval-print-last-sexp))
        ((eq major-mode 'cider-clojure-interaction-mode)
         (setq this-command 'cider-eval-print-last-sexp)
         (cider-eval-print-last-sexp))
        ((eq major-mode 'cider-repl-mode)
         (setq this-command 'cider-repl-newline-and-indent)
         (cider-repl-newline-and-indent))
        ((eq major-mode 'inferior-emacs-lisp-mode)
         (lispy-newline-and-indent-plain))
        ((lispy-left-p)
         (skip-chars-backward ",@'`#")
         (newline-and-indent)
         (skip-chars-forward ",@'`#")
         (indent-sexp))
        (t
         (lispy-newline-and-indent-plain))))

(declare-function cider-repl-return "ext:cider-repl")
(declare-function slime-repl-return "ext:slime-repl")
(declare-function sly-mrepl-return "ext:sly-mrepl")
(declare-function racket-repl-submit "ext:racket-repl")
(defun lispy-newline-and-indent-plain ()
  "When in minibuffer, exit it.  Otherwise forward to `newline-and-indent'."
  (interactive)
  (if (minibufferp)
      (exit-minibuffer)
    (cl-case major-mode
      (cider-repl-mode
       (cider-repl-return))
      (slime-repl-mode
       (slime-repl-return))
      (sly-mrepl-mode
       (sly-mrepl-return))
      (comint-mode
       (comint-send-input))
      (python-mode
       (newline-and-indent))
      (inferior-emacs-lisp-mode
       (setq this-command 'ielm-return)
       (ielm-return))
      (racket-repl-mode
       (racket-repl-submit))
      (t
       (if (and (not (lispy--in-string-or-comment-p))
                (if (memq major-mode lispy-clojure-modes)
                    (lispy-looking-back "[^#`'@~][#`'@~]+")
                  (lispy-looking-back "[^#`',@|][#`',@]+")))
           (save-excursion
             (goto-char (match-beginning 0))
             (newline-and-indent))
         (newline-and-indent))
       (let ((lispy-ignore-whitespace t))
         (save-excursion
           (lispy--out-backward 1)
           (unless (< 50000
                      (- (save-excursion (forward-list 1))
                         (point)))
             (indent-sexp))))))))

;;* Globals: miscellanea
(defun lispy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (eq (char-before) ?\")
    (backward-char 1))
  (let (bnd str)
    (setq str (lispy--string-dwim (setq bnd (lispy--bounds-string))))
    (delete-region (car bnd) (cdr bnd))
    (insert (replace-regexp-in-string "\n" "\\\\n" str))))

;;* Locals: navigation
;;* Locals: Paredit transformations
(defun lispy--sub-slurp-forward (arg)
  "Grow current marked symbol by ARG words forwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-at "\\s_")
    (let ((end (cdr (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (forward-word 1)
        (when (> (point) end)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy--sub-slurp-backward (arg)
  "Grow current marked symbol by ARG backwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (lispy-looking-back "\\s_")
    (let ((beg (car (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (backward-word 1)
        (when (< (point) beg)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy-slurp (arg)
  "Grow current sexp by ARG sexps.
If ARG is zero, grow as far as possible. If ARG is -1, grow until the end or
beginning of the line. If it is not possible to slurp to the end of the line,
slurp as far as possible within the line. If before a multi-line list, slurp to
the end of the line where that list ends."
  (interactive "p")
  (if (region-active-p)
      (if (= (point) (region-end))
          (cond ((= arg 0)
                 (while (and (lispy-dotimes 1 (forward-sexp 1))
                             (not (looking-at "\\'")))))
                ((= arg -1)
                 (while (and (not (looking-at (concat lispy-right "*$")))
                             (lispy-dotimes 1 (forward-sexp 1)))))
                ((or (looking-at "\\s_")
                     (save-excursion
                       (goto-char (region-beginning))
                       (and (not (lispy-left-p))
                            (lispy-looking-back "\\s_"))))
                 (lispy--sub-slurp-forward arg))
                ((looking-at "[\n ]+;")
                 (goto-char (match-end 0))
                 (goto-char (cdr (lispy--bounds-comment))))
                (t
                 (lispy-dotimes arg
                   (forward-sexp 1))))
        (cond ((= arg 0)
               (while (and (lispy-dotimes 1 (forward-sexp -1))
                           (not (looking-at "\\`")))))
              ((= arg -1)
               (while (and (not (lispy-looking-back "^[[:space:]]*"))
                           (lispy-dotimes 1 (forward-sexp -1)))))
              ((or (and (not (lispy-left-p))
                        (lispy-looking-back "\\s_"))
                   (save-excursion
                     (goto-char (region-end))
                     (looking-at "\\s_")))
               (lispy--sub-slurp-backward arg))
              ((save-excursion
                 (skip-chars-backward " \n")
                 (lispy--in-comment-p))
               (skip-chars-backward " \n")
               (goto-char (car (lispy--bounds-comment))))
              (t
               (lispy-dotimes arg
                 (forward-sexp -1)))))
    (if (lispy-right-p)
        (cond ((= arg 0)
               (let ((last-pos (point)))
                 (while (and (lispy-dotimes 1
                               (lispy--slurp-forward)
                               (lispy--reindent))
                             (not (= (point) last-pos)))
                   (setq last-pos (point)))))
              ((= arg -1)
               (while (and (not (looking-at (concat "\\("
                                                    lispy-right
                                                    "\\|$\\)")))
                           (lispy-dotimes 1
                             (lispy--slurp-forward)))))
              (t
               (lispy-dotimes arg
                 (lispy--slurp-forward))))
      (if (lispy-left-p)
          (cond ((= arg 0)
                 ;; lispy--slurp-backward errors when reaching another delimiter
                 (while (and (lispy-dotimes 1
                               (lispy--slurp-backward))
                             (not (lispy-looking-back "\\`")))))
                ((= arg -1)
                 (while (and (not (lispy-looking-back "^[[:space:]]*"))
                             (lispy-dotimes 1
                               (lispy--slurp-backward)))))
                (t
                 (lispy-dotimes arg
                   (lispy--slurp-backward))))))
    (lispy--reindent)))

(defun lispy-down-slurp ()
  "Move current sexp or region into the next sexp."
  (interactive)
  (let ((bnd (lispy--bounds-dwim))
        (leftp (lispy--leftp))
        (regionp (region-active-p))
        (bolp (bolp))
        deactivate-mark)
    (when (lispy-left-p)
      (forward-sexp))
    (let ((pt (save-excursion
                (when (lispy-forward 1)
                  (lispy-backward 1)
                  (point)))))
      (when pt
        (goto-char pt)
        (lispy--teleport (car bnd) (cdr bnd) (not leftp) regionp)
        (save-excursion
          (backward-char 1)
          (when (lispy-looking-back (concat lispy-right " +"))
            (just-one-space))
          (when (and bolp (lispy-looking-back "^ +"))
            (delete-region (match-beginning 0)
                           (match-end 0)))
          (indent-sexp))))))

(defun lispy-up-slurp ()
  "Move current sexp or region into the previous sexp.
If the point is by itself on a line or followed only by right delimiters, slurp
the point into the previous list. This can be of thought as indenting the code
to the next level and adjusting the parentheses accordingly."
  (interactive)
  (let* ((empty-line-p (lispy--empty-line-p))
         (list-start (when (eq empty-line-p 'right)
                       (save-excursion
                         (re-search-forward lispy-right)
                         (lispy-other)
                         (point))))
         (failp (when list-start
                  (= list-start
                     (save-excursion
                       (re-search-backward lispy-left)
                       (point)))))
         (bnd (if empty-line-p
                  (cons (point) (point))
                (lispy--bounds-dwim)))
         (regionp (region-active-p))
         (endp (or (lispy-right-p)
                   (and (region-active-p) (= (point) (region-end)))))
         p-beg p-end
         (deactivate-mark nil)
         bsize)
    (deactivate-mark)
    (goto-char (car bnd))
    (if (or failp
            (not (lispy-backward 1)))
        (progn
          (lispy--complain "No list above to slurp into")
          (if regionp
              (lispy--mark bnd)
            (goto-char
             (if endp
                 (cdr bnd)
               (car bnd)))))
      (setq p-beg (point))
      (forward-list)
      (setq p-end (point))
      (goto-char (car bnd))
      (setq bsize (buffer-size))
      (lispy-save-excursion
        (goto-char (cdr bnd))
        (insert (char-before p-end))
        (goto-char p-end)
        (backward-delete-char 1)
        (goto-char p-beg)
        (indent-sexp))
      (setq bnd (cons (point)
                      (+ (point)
                         (- (cdr bnd) (car bnd))
                         (- (buffer-size)
                            bsize
                            (- (point) (car bnd))
                            1))))
      (when regionp
        (lispy--mark bnd))
      (if endp
          (goto-char (cdr bnd))
        (if (region-active-p)
            (lispy-other)
          (goto-char (car bnd)))))))

(defun lispy--backward-sexp-or-comment ()
  "When in comment, move to the comment start.
Otherwise, move to the previous sexp."
  (if (lispy--in-comment-p)
      (goto-char (car (lispy--bounds-comment)))
    (forward-sexp -1))
  (skip-chars-backward " \n"))

(defun lispy--forward-sexp-or-comment ()
  "When before comment, move to the comment end.
Otherwise, move to the next sexp."
  (if (save-excursion
        (skip-chars-forward " \n")
        (lispy--in-comment-p))
      (progn
        (skip-chars-forward " \n")
        (goto-char (cdr (lispy--bounds-comment))))
    (forward-sexp 1)))

(defun lispy-barf (arg)
  "Shrink current sexp or region by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let* ((bnd (lispy--bounds-dwim))
                (str (lispy--string-dwim bnd))
                (one-symbolp (lispy--symbolp str)))
           (if (= (point) (region-end))
               (cond (one-symbolp
                      (lispy-dotimes arg
                        (if (re-search-backward "\\sw\\s_+" (region-beginning) t)
                            (forward-char 1)
                          (throw 'result i))))
                     ((lispy--in-comment-p)
                      (goto-char (car (lispy--bounds-comment)))
                      (if (= (point) (region-beginning))
                          (goto-char (cdr (lispy--bounds-comment)))
                        (skip-chars-backward " \n")))
                     (t
                      (cl-incf arg)
                      (lispy-dotimes arg
                        (lispy--backward-sexp-or-comment))
                      (when (< (point) (car bnd))
                        (goto-char (car bnd)))
                      (lispy--forward-sexp-or-comment)))
             (cond (one-symbolp
                    (lispy-dotimes arg
                      (if (re-search-forward "\\s_+\\sw" (region-end) t)
                          (backward-char 1)
                        (throw 'result i))))
                   ((lispy--in-comment-p)
                    (goto-char (cdr (lispy--bounds-comment)))
                    (if (= (region-beginning) (region-end))
                        (goto-char (car bnd))
                      (skip-chars-forward " \n")))
                   (t
                    (save-restriction
                      (narrow-to-region (point-min)
                                        (region-end))
                      (cl-incf arg)
                      (lispy-dotimes arg
                        (lispy--forward-sexp-or-comment))
                      (if (lispy--in-comment-p)
                          (goto-char (car (lispy--bounds-comment)))
                        (forward-sexp -1))
                      (widen)))))))

        ((looking-at "()"))

        ((lispy-right-p)
         (lispy-dotimes arg
           (lispy--barf-backward)))

        ((lispy-left-p)
         (lispy-dotimes arg
           (lispy--barf-forward)))))

(defun lispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (lispy-dotimes arg
    (let ((bnd (lispy--bounds-dwim))
          (deactivate-mark nil))
      (cond ((region-active-p)
             (save-excursion
               (goto-char (cdr bnd))
               (re-search-backward lispy-right)
               (delete-region (point) (cdr bnd)))
             (save-excursion
               (goto-char (car bnd))
               (re-search-forward lispy-left)
               (delete-region (car bnd) (point))))
            ((lispy-splice-let))

            ((lispy-left-p)
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (lispy--delete-leading-garbage)
             (delete-char 1)
             (lispy-forward 1)
             (lispy-backward 1))

            ((lispy-right-p)
             (setq bnd (lispy--bounds-dwim))
             (delete-char -1)
             (goto-char (car bnd))
             (let ((pt (point)))
               (re-search-forward lispy-left nil t)
               (delete-region pt (point)))
             (lispy-backward 1)
             (forward-list))

            (t
             (setq bnd (lispy--bounds-list))
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (save-excursion
               (goto-char (car bnd))
               (delete-char 1)))))))

(defun lispy-find (item tree)
  (cond ((null tree)
         nil)
        ((consp tree)
         (or (lispy-find item (car tree))
             (lispy-find item (cdr tree))))
        (t
         (eq item tree))))

(defun lispy-splice-let ()
  "Join the current `let' into the parent `let'."
  (when (save-excursion
          (and (looking-at "(let")
               (lispy--out-backward 1)
               (looking-at "(let")))
    (if (memq major-mode lispy-clojure-modes)
        (lispy-splice-let-clojure)
      (let ((child-binds (save-excursion
                           (lispy-step-in 1)
                           (read (lispy--string-dwim))))
            (parent-binds
             (mapcar (lambda (x) (if (consp x) (car x) x))
                     (save-excursion
                       (lispy-up 1)
                       (read (lispy--string-dwim)))))
            (end (save-excursion
                   (lispy-step-in 2)
                   (point)))
            (beg (save-excursion
                   (lispy-up 1)
                   (lispy-other)
                   (1- (point)))))
        (save-excursion
          (forward-list)
          (delete-char -1))
        (delete-region beg end)
        (when parent-binds
          (newline-and-indent))
        (lispy-step-out 2)
        (when (cl-find-if (lambda (v) (lispy-find v child-binds))
                          parent-binds)
          (cond
            ((looking-at "(let\\*"))
            ((looking-at "(\\(let\\)")
             (replace-match "(let*")
             (lispy--out-backward 1)
             (indent-sexp))
            (t
             (error "unexpected"))))
        (lispy--prettify-1)
        (lispy-step-in 2)
        (when parent-binds
          (lispy-down (length parent-binds))))
      t)))

(defun lispy-splice-let-clojure ()
  "Join the current Clojure `let' form into the parent `let'."
  (let ((end (save-excursion
               (lispy-step-in 1)
               (1+ (point))))
        (beg (save-excursion
               (lispy-up 1)
               (lispy-other)
               (1- (point)))))
    (save-excursion
      (forward-list)
      (delete-char -1))
    (delete-region beg end)
    (insert "\n")
    (lispy--out-backward 2)
    (lispy--prettify-1)
    t))

(defun lispy-raise (arg)
  "Use current sexp or region as replacement for its parent.
Do so ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((regionp (region-active-p))
          (leftp (lispy--leftp))
          (deactivate-mark nil)
          bnd1 bnd2)
      ;; re-indent first
      (lispy-save-excursion (lispy--out-forward 1))
      (unless leftp
        (lispy-other))
      (setq bnd1 (lispy--bounds-dwim))
      (deactivate-mark)
      (lispy--out-forward 1)
      (setq bnd2 (lispy--bounds-dwim))
      (delete-region (cdr bnd2) (cdr bnd1))
      (delete-region (car bnd2) (car bnd1))
      (if regionp
          (progn
            (indent-region (car bnd2) (point))
            (lispy--mark (cons (car bnd2) (point))))
        (lispy-from-left
         (indent-sexp)))
      (unless (eq leftp (lispy--leftp))
        (lispy-other)))))

(defun lispy-raise-some ()
  "Use current sexps as replacement for their parent.
The outcome when ahead of sexps is different from when behind."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((lispy-left-p)
           (if (null (lispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lispy--complain "Not enough depth to raise"))
             (backward-char 1)
             (set-mark (point))
             (goto-char pt)))

          ((lispy-right-p)
           (if (null (lispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lispy--complain "Not enough depth to raise"))
             (backward-list)
             (forward-char 1)
             (set-mark (point))
             (goto-char pt)))

          (t
           (error "Unexpected")))
    (lispy-raise 1)
    (deactivate-mark)))

(defun lispy-convolute (arg)
  "Replace (...(,,,|( with (,,,(...|( where ... and ,,, is arbitrary code.
When ARG is more than 1, pull ARGth expression to enclose current sexp.
When ARG is nil, convolute only the part above sexp."
  (interactive "p")
  (let ((deactivate-mark nil)
        (only-upper nil))
    (when (= arg 0)
      (setq only-upper t)
      (setq arg 1))
    (if (and (save-excursion
               (lispy--out-forward (1+ arg)))
             (save-excursion
               (lispy--out-backward (1+ arg))))
        (let (beg end)
          (lispy-from-left
           (setq beg (point))
           (setq end (lispy--out-backward arg))
           (lispy--out-backward 1)
           (lispy--swap-regions (cons beg end)
                                (cons (point) (point)))
           (lispy--reindent arg))
          (unless only-upper
            (lispy-from-left
             (lispy-other)
             (setq beg (point))
             (setq end (lispy--out-forward arg))
             (lispy--out-forward 1)
             (lispy--swap-regions (cons beg end)
                                  (cons (point) (point)))
             (ignore-errors
               (lispy-other))
             (lispy--reindent (1+ arg)))))
      (error "Not enough depth to convolute"))))

(defvar lispy-repeat--command nil
  "Command to use with `lispy-repeat'.")

(defvar lispy-repeat--prefix-arg nil
  "Prefix arg to use with `lispy-repeat'.")

(defun lispy-repeat ()
  "Repeat last command with last prefix arg."
  (interactive)
  (unless (memq last-command
                '(special-lispy-repeat lispy-repeat))
    (setq lispy-repeat--command last-command)
    (setq lispy-repeat--prefix-arg
          (or last-prefix-arg 1)))
  (setq current-prefix-arg lispy-repeat--prefix-arg)
  (funcall lispy-repeat--command))

(defun lispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point))
        bnd)
    (cond ((lispy-right-p)
           (when (lispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lispy--out-forward 1)
             (lispy--reindent 1)))
          ((lispy-left-p)
           (when (lispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lispy-save-excursion
               (forward-char 1)
               (lispy-step-out 2)
               (lispy--prettify-1))))
          ((and (setq bnd (lispy--bounds-string))
                (or (save-excursion
                      (goto-char (car bnd))
                      (skip-chars-backward " \t\n")
                      (when (eq (char-before) ?\")
                        (delete-region (1- (point))
                                       (1+ (car bnd)))
                        t))
                    (save-excursion
                      (goto-char (cdr bnd))
                      (skip-chars-forward " \t\n")
                      (when (looking-at "\"")
                        (delete-region (1- (cdr bnd))
                                       (1+ (point)))
                        t))))))))

(defun lispy-split ()
  "Split sexps."
  (interactive)
  (let (bnd
        char-left
        char-right)
    (cond ((lispy--in-comment-p)
           (indent-new-comment-line))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (insert "\"\"")
           (when (eolp)
             (delete-char 1))
           (backward-char)
           (newline-and-indent))
          ((lispy-split-let-binding))
          (t
           (when (save-excursion
                   (prog1 (lispy--out-forward 1)
                     (setq char-right (char-before))
                     (forward-list -1)
                     (setq char-left (char-after))))
             (insert (string char-right char-left))
             (backward-char 2)
             (lispy-right 1))
           (newline-and-indent)
           (when (lispy-left-p)
             (indent-sexp))))))

(defun lispy-split-let-binding ()
  (when (and
         (or (lispy-left-p)
             (lispy-right-p))
         (save-excursion
           (lispy--out-backward 2)
           (looking-at "(let")))
    (save-excursion
      (lispy--out-forward 2)
      (insert ")"))
    (save-excursion
      (insert ")\n(let (")
      (lispy--out-backward 3)
      (lispy--prettify-1))
    (lispy-step-in 1)
    (lispy-down 1)
    t))

;;* Locals: more transformations
(defun lispy-move-up (arg)
  "Move current expression up ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (lispy-left-p)
          (lispy-right-p)
          (region-active-p)
          (looking-at lispy-outline))
      (lispy--move-up-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (lispy--out-backward 1)
                     (point)))))
      (lispy--move-up-special arg)
      (forward-char offset))))

(defun lispy-move-down (arg)
  "Move current expression down ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (lispy-left-p)
          (lispy-right-p)
          (region-active-p)
          (looking-at lispy-outline))
      (lispy--move-down-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (lispy--out-backward 1)
                     (point)))))
      (lispy--move-down-special arg)
      (forward-char offset))))

(defun lispy--move-up-region (arg)
  "Swap the marked region ARG positions up.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (looking-at "\\_<")
          (save-excursion
            (goto-char (region-end))
            (looking-at "-"))))
    ((lispy-after-string-p "-")
     (let ((bnd1 (lispy--bounds-dwim))
           bnd2)
       (lispy-up arg)
       (setq bnd2 (lispy--bounds-dwim))
       (lispy--swap-regions bnd1 bnd2)
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (cdr bnd1) (car bnd1)))))
    ((= arg 1)
     (let ((bnd1 (lispy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (lispy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (car bnd1))
       (if (re-search-backward "[^ \t\n`'#({[]" (car bnd0) t)
           (progn
             (deactivate-mark)
             (if (lispy--in-comment-p)
                 (setq bnd2 (lispy--bounds-comment))
               (when (eq (char-after) ?\")
                 (forward-char)
                 (backward-sexp))
               (when (memq (char-after) '(?\) ?\] ?\}))
                 (forward-char))
               (setq bnd2 (lispy--bounds-dwim)))
             (lispy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (car bnd2))
             (set-mark (point))
             (forward-char (- (cdr bnd1) (car bnd1))))
         (setq deactivate-mark nil)
         (lispy--mark bnd1)))
     (exchange-point-and-mark))
    (t
     (let ((bnd1 (lispy--bounds-dwim)))
       (lispy-up arg)
       (lispy--mark
        (car
         (lispy--swap-regions
          bnd1 (lispy--bounds-dwim)))))
     (exchange-point-and-mark))))

(defun lispy--move-up-special (arg)
  "Move current expression up ARG times.  Don't exit parent list."
  (let ((at-start (lispy--leftp)))
    (unless (or at-start (looking-at lispy-outline))
      (lispy-other))
    (cond ((region-active-p)
           (lispy--move-up-region arg))
          (t
           (lispy--mark (lispy--bounds-dwim))
           (lispy-move-up arg)
           (deactivate-mark)
           (lispy-other)))
    (unless at-start (lispy-other))))

(defun lispy--move-down-region (arg)
  "Swap the marked region ARG positions down.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (lispy-after-string-p "-")
          (save-excursion
            (goto-char (region-end))
            (looking-at "\\_>"))))
    ((save-excursion
       (goto-char (region-end))
       (looking-at "-"))
     (let ((bnd1 (lispy--bounds-dwim))
           bnd2)
       (lispy-down arg)
       (setq bnd2 (lispy--bounds-dwim))
       (lispy--swap-regions bnd1 bnd2)
       (goto-char (cdr bnd2))
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (car bnd1) (cdr bnd1)))))
    ((= arg 1)
     (let ((bnd1 (lispy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (lispy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (cdr bnd1))
       (if (re-search-forward "[^ \t\n]" (max (1- (cdr bnd0))
                                              (point)) t)
           (progn
             (deactivate-mark)
             (if (lispy--in-comment-p)
                 (setq bnd2 (lispy--bounds-comment))
               (when (memq (char-before) '(?\( ?\" ?\[ ?\{))
                 (backward-char))
               (setq bnd2 (lispy--bounds-dwim)))
             (lispy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (cdr bnd2))
             (set-mark (point))
             (backward-char (- (cdr bnd1) (car bnd1))))
         (lispy--mark bnd1)
         (exchange-point-and-mark))))
    (t
     (let ((bnd1 (lispy--bounds-dwim)))
       (lispy-down arg)
       (lispy--mark
        (cdr
         (lispy--swap-regions
          bnd1 (lispy--bounds-dwim))))
       (lispy-other)))))

(defun lispy--move-down-special (arg)
  "Move current expression down ARG times.  Don't exit parent list."
  (let ((at-start (lispy--leftp)))
    (unless (or at-start (looking-at lispy-outline))
      (lispy-other))
    (cond ((region-active-p)
           (lispy--move-down-region arg))
          ((looking-at lispy-outline)
           (lispy-dotimes arg
             (let ((bnd1 (lispy--bounds-outline))
                   bnd2)
               (goto-char (1+ (cdr bnd1)))
               (if (and (setq bnd2 (lispy--bounds-outline))
                        (not (equal bnd1 bnd2)))
                   (progn
                     (lispy--swap-regions bnd1 bnd2)
                     (forward-char (1+ (- (cdr bnd2) (car bnd2)))))
                 (goto-char (car bnd1))))))
          (t
           (lispy--mark (lispy--bounds-dwim))
           (lispy-move-down arg)
           (deactivate-mark)
           (lispy-other)))
    (unless at-start (lispy-other))))

(defun lispy-move-left (arg)
  "Move region left ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (lispy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (deactivate-mark)
        (lispy--out-backward 1)
        (setq pt (point))
        (insert str)
        (newline-and-indent)
        (skip-chars-backward " \n")
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark nil)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-other)))))))

(defun lispy-move-right (arg)
  "Move region right ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (lispy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (lispy--out-backward 1)
        (deactivate-mark)
        (lispy-other)
        (newline-and-indent)
        (setq pt (point))
        (insert str)
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark nil)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-other)))))))

(defun lispy-clone (arg)
  "Clone sexp ARG times.
When the sexp is top level, insert an additional newline."
  (interactive "p")
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (pt (point)))
    (cond ((region-active-p)
           (lispy-dotimes arg
             (cl-labels
                 ((doit ()
                    (let (deactivate-mark)
                      (save-excursion
                        (newline)
                        (insert str)
                        (lispy--indent-for-tab)))))
               (if (= (point) (region-end))
                   (doit)
                 (exchange-point-and-mark)
                 (doit)
                 (exchange-point-and-mark)))))
          ((lispy-left-p)
           (goto-char (car bnd))
           (cond ((and (bolp) (looking-at "(defun"))
                  (lispy-dotimes arg
                    (insert str)
                    (newline)
                    (newline))
                  (goto-char pt))
                 ((and (bolp)
                       (save-excursion
                         (goto-char (cdr bnd))
                         (looking-at "\n;; =>")))
                  (lispy-dotimes arg
                    (insert str)
                    (newline-and-indent)
                    (lispy-move-down 1)))
                 (t
                  (lispy-dotimes arg
                    (insert str)
                    (newline-and-indent))
                  (goto-char pt))))
          ((lispy-right-p)
           (if (save-excursion
                 (backward-list)
                 (and (bolp) (looking-at "(defun")))
               (lispy-dotimes arg
                 (newline)
                 (newline-and-indent)
                 (insert str))
             (lispy-dotimes arg
               (newline-and-indent)
               (insert str))))
          (t
           (error "Unexpected")))))

(defvar lispy--oneline-comments nil
  "Collect comments for `lispy--oneline'.")

(defun lispy-mapcan-tree (func expr)
  "Reduce with FUNC all lists in EXPR."
  (cond ((null expr)
         nil)
        ((and (vectorp expr) (> (length expr) 0))
         (apply #'vector
                (funcall func
                         (lispy-mapcan-tree func (aref expr 0))
                         (lispy-mapcan-tree
                          func
                          (cdr
                           (mapcar #'identity expr))))))
        ((listp expr)
         (funcall func
                  (lispy-mapcan-tree func (car expr))
                  (lispy-mapcan-tree func (cdr expr))))
        (t
         expr)))

(defun lispy--oneline (expr &optional ignore-comments)
  "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
  (lispy-mapcan-tree
   (lambda (x y)
     (cond ((equal x '(ly-raw newline))
            y)
           ((lispy--raw-comment-p x)
            (if (null ignore-comments)
                (progn
                  (push x lispy--oneline-comments)
                  y)
              (if (equal (car y) '(ly-raw newline))
                  (cons x y)
                `(,x (ly-raw newline) ,@y))))
           ((and (lispy--raw-string-p x)
                 (null ignore-comments))
            (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (cl-caddr x)))
                  y))
           (t
            (cons x y))))
   expr))

(defun lispy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (cond ((lispy--in-comment-p)
         (let* ((bnd (lispy--bounds-comment))
                (str (lispy--string-dwim bnd)))
           (delete-region (car bnd) (cdr bnd))
           (insert (lispy-comment-char 2 " ")
                   (mapconcat #'identity
                              (split-string str "[ \n]*;;[ \n]*" t)
                              " "))
           (beginning-of-line)
           (back-to-indentation)))
        ((and (region-active-p)
              (= (char-after (region-beginning)) ?\")
              (= (char-before (region-end)) ?\"))
         (lispy-string-oneline))
        (t
         (unless (or (lispy-left-p)
                     (lispy-right-p))
           (lispy--out-backward 1))
         (let* ((bnd (lispy--bounds-dwim))
                (str (lispy--string-dwim bnd))
                (from-left (lispy-left-p))
                expr)
           (delete-region (car bnd) (cdr bnd))
           (when (region-active-p)
             (deactivate-mark))
           (setq lispy--oneline-comments nil)
           (if (setq expr (ignore-errors
                            (lispy--oneline
                             (lispy--read str))))
               (progn
                 (mapc (lambda (x)
                         (lispy--insert x)
                         (newline))
                       lispy--oneline-comments)
                 (lispy--insert expr))
             (let ((no-comment "")
                   comments)
               (cl-loop for s in (split-string str "\n" t)
                        do (if (string-match "^ *\\(;\\)" s)
                               (push (substring s (match-beginning 1)) comments)
                             (setq no-comment (concat no-comment "\n" s))))
               (when comments
                 (insert (mapconcat #'identity comments "\n") "\n"))
               (insert (substring
                        (replace-regexp-in-string "\n *" " " no-comment) 1))))
           (when from-left
             (backward-list))))))

(defun lispy-multiline (&optional arg)
  "Spread current sexp over multiple lines.
When ARG is `fill', do nothing for short expressions."
  (interactive "p")
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (lispy-from-left
   (let* ((bnd (lispy--bounds-list))
          (str (lispy--string-dwim bnd))
          (plain-expr (read str))
          (expr (lispy--read str))
          res)
     (unless (and (eq arg 'fill)
                  (< (length str) 80))
       (unless (listp plain-expr)
         (setq plain-expr nil))
       (if (or (cl-some #'listp plain-expr)
               (member '(ly-raw newline) expr))
           (let ((pt (point)))
             (lispy-forward 1)
             (while (and (lispy-step-in 1) (> (point) pt))
               (unless (looking-at "\]\\|)\\|\n")
                 (when (looking-at " *")
                   (replace-match "\n")
                   (backward-char 1))))
             (goto-char pt)
             (indent-sexp))
         (delete-region (car bnd) (cdr bnd))
         (setq res
               (butlast
                (cl-mapcan (lambda (y)
                             (if (memq y '(ly-raw clojure-map clojure-set))
                                 (list y)
                               (list y '(ly-raw newline))))
                           (lispy--read str))))
         (when (vectorp expr)
           (setq res (apply #'vector res)))
         (lispy--insert res))))))

(defvar-local lispy--multiline-take-3
    '(defvar defun defmacro defcustom defgroup defvar-local declare-function
      define-key nth throw define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-3 '())

(defvar lispy--multiline-take-3-arg
  '(defun defmacro declare-function define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.
The third one is assumed to be the arglist and will not be changed.")

(defvar-local lispy--multiline-take-2
    '(defface define-minor-mode
      condition-case while incf car
      cdr > >= < <= /= = eq equal incf
      decf cl-incf cl-decf catch
      require provide setq cons when
      if unless interactive assq delq
      assoc declare lambda remq
      make-variable-buffer-local
      bound-and-true-p
      called-interactively-p
      lispy-dotimes cond case cl-case
      defalias 1+ 1- dotimes dolist boundp fboundp macrop
      null consp oddp zerop plusp minusp kbd
      not pop listp or and)
  "List of constructs for which the first 2 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-2 '(loop recur for fn def defn ns if -> ->>
                           + +' - -' * *' / > >= < <= = ==
                           or and not
                           assoc! assoc assoc-in concat))

(defvar lispy--multiline-take-2-arg '(declare lambda
                                      make-variable-buffer-local
                                      bound-and-true-p
                                      called-interactively-p
                                      lispy-dotimes dotimes)
  "List of constructs for which the first 2 elements are on the first line.
The second one will not be changed.")

(defun lispy-interleave (x lst &optional step)
  "Insert X in between each element of LST.
Don't insert X when it's already there.
When STEP is non-nil, insert in between each STEP elements instead."
  (setq step (or step 1))
  (let ((res (nreverse (lispy-multipop lst step)))
        item)
    (while lst
      (unless (equal (car res) x)
        (push x res))
      (unless (equal (car res)
                     (car (setq item (lispy-multipop lst step))))
        (setq res (nconc (nreverse item) res))))
    (nreverse res)))

(defcustom lispy-multiline-threshold 32
  "Don't multiline expresssions shorter than this when printed as a string."
  :type 'integer)

(defun lispy--translate-newlines (str)
  "Replace quoted newlines with real ones in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "\\\\n" nil t)
      (unless (= ?\\
                 (char-before (- (point) 2)))
        (replace-match "\n" nil t)))
    (buffer-string)))

(defun lispy--multiline-1 (expr &optional quoted)
  "Transform a one-line EXPR into a multi-line.
When QUOTED is not nil, assume that EXPR is quoted and ignore some rules."
  (cond ((vectorp expr)
         (apply #'vector
                (lispy--multiline-1
                 (mapcar #'identity expr))))
        ((not (listp expr))
         expr)
        ((and lispy-multiline-threshold
              (< (length (lispy--prin1-to-string
                          expr 0 'emacs-lisp-mode))
                 lispy-multiline-threshold))
         expr)
        ((and (eq (car-safe expr) 'ly-raw)
              (memq (cadr expr) '(clojure-map clojure-set)))
         (list 'ly-raw (cadr expr)
               (lispy-interleave '(ly-raw newline)
                                 (mapcar #'lispy--multiline-1 (cl-caddr expr))
                                 2)))
        ((and (eq (car-safe expr) 'ly-raw)
              (eq (nth 1 expr) 'splice))
         (list 'ly-raw (nth 1 expr) (nth 2 expr) (nth 3 expr)
               (lispy-interleave '(ly-raw newline)
                                 (mapcar #'lispy--multiline-1 (car (nthcdr 4 expr)))
                                 2)))
        (t
         (let ((res nil)
               elt)
           (while expr
             (setq elt (pop expr))
             (when (equal elt '(ly-raw clojure-symbol "let"))
               (setq elt 'let))
             (cond
               ((eq elt 'ly-raw)
                (cl-case (car expr)
                  (empty
                   (setq res '(ly-raw empty)))
                  (raw
                   (setq res (cons elt expr)))
                  (dot
                   (setq res (cons elt expr)))
                  (newline
                   (setq res '(ly-raw newline)))
                  (comment
                   (setq res (cons elt expr)))
                  (string
                   (setq res
                         `(ly-raw string
                                  ,(lispy--translate-newlines
                                    (cadr expr)))))
                  (t (unless (= (length expr) 2)
                       (error "Unexpected expr: %S" expr))
                     (unless (null res)
                       (error "Stray ly-raw in %S" expr))
                     (setq res (list 'ly-raw (car expr)
                                     (lispy--multiline-1
                                      (cadr expr)
                                      (car (memq (car expr) '(quote \` clojure-lambda))))))))
                (setq expr nil))
               ((vectorp elt)
                (push
                 (apply #'vector
                        (lispy--multiline-1
                         (mapcar #'identity elt)))
                 res)
                (push '(ly-raw newline) res))
               ((equal elt '(ly-raw dot))
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (push elt res))
               ((equal elt '(ly-raw clojure-comma))
                ;; two sexps without newlines, then a comma with a newline
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (when (equal (cadr res) '(ly-raw newline))
                  (setq res
                        (cons (car res)
                              (cddr res))))
                (push elt res)
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-3))
                (push elt res)
                ;; name
                (when expr
                  (push (pop expr) res))
                ;; value
                (when expr
                  (if (memq elt lispy--multiline-take-3-arg)
                      (push (pop expr) res)
                    (push (car (lispy--multiline-1 (list (pop expr)))) res)))
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-2))
                (push elt res)
                (when (memq elt lispy--multiline-take-2-arg)
                  (push (pop expr) res)
                  (push '(ly-raw newline) res)))
               ((and (memq elt '(let let*))
                     expr
                     (or (memq major-mode lispy-clojure-modes)
                         (and
                          (listp (car expr))
                          (listp (cdar expr)))))
                (push elt res)
                (let ((body (pop expr)))
                  (push
                   (if (memq major-mode lispy-clojure-modes)
                       (apply #'vector
                              (lispy-interleave '(ly-raw newline)
                                                (mapcar #'lispy--multiline-1 body) 2))
                     (lispy-interleave
                      '(ly-raw newline)
                      (mapcar
                       (lambda (x)
                         (if (and (listp x)
                                  (not (eq (car x) 'ly-raw)))
                             (cons (car x)
                                   (lispy--multiline-1 (cdr x)))
                           x))
                       body)))
                   res))
                (push '(ly-raw newline) res))
               ((keywordp elt)
                (push elt res))
               ((not (listp elt))
                (push elt res)
                (unless (and (numberp elt) (eq quoted 'clojure-lambda))
                  (push '(ly-raw newline) res)))
               (t
                (setq elt (lispy--multiline-1 elt))
                (if (equal elt '(ly-raw newline))
                    (unless (equal elt (car res))
                      (push elt res))
                  (push elt res)
                  (push '(ly-raw newline) res)))))
           (cond ((equal (car res) 'ly-raw)
                  res)
                 ((equal (car res) '(ly-raw newline))
                  (if (and (cdr res)
                           (lispy--raw-comment-p (cadr res)))
                      (nreverse res)
                    (nreverse (cdr res))))
                 (t
                  (nreverse res)))))))

(defun lispy-alt-multiline (&optional silent)
  "Spread current sexp over multiple lines.
When SILENT is non-nil, don't issue messages."
  (interactive)
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (expr-o (lispy--oneline expr t))
         (expr-m (lispy--multiline-1 expr-o))
         (leftp (lispy--leftp))
         (print-circle nil))
    (cond ((equal expr expr-m)
           (unless silent
             (message "No change")))
          ((and (memq major-mode lispy-elisp-modes)
                (not
                 (condition-case nil
                     (equal (read str)
                            (read (lispy--prin1-to-string
                                   expr-m 0 major-mode)))
                   (error
                    (lispy--complain "Got an unreadable expr (probably overlay)")
                    t))))
           (error "Got a bad transform: %S" expr-m))
          (t
           (delete-region (car bnd) (cdr bnd))
           (lispy--insert expr-m)
           (when leftp
             (backward-list))))))

(defvar lispy-do-fill nil
  "If t, `lispy-insert-1' will try to fill.")

(defcustom lispy-move-after-commenting t
  "When non-nil, adjust point to next sexp after commenting out a
  sexp. If at last sexp in list, move out and backwards to
  enclosing sexp."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-comment-use-single-semicolon nil
  "When non-nil, prefer single semicolons for comments at the
  right of the source code (after lispy-right or at eol)."
  :type 'boolean
  :group 'lispy)

(defun lispy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (> arg 1) (lispy--in-comment-p))
      (let ((bnd (lispy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (lispy-dotimes arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((lispy--in-string-or-comment-p)
               (cond ((and (eq major-mode 'emacs-lisp-mode)
                           (lispy-after-string-p ";;; "))
                      (delete-char -1)
                      (insert "###autoload")
                      (forward-char 1))
                     ((lispy-after-string-p (lispy-comment-char 2 " "))
                      (backward-char 1)
                      (insert (lispy-comment-char))
                      (forward-char 1))
                     ((and lispy-comment-use-single-semicolon
                           (lispy-after-string-p (lispy-comment-char 1 " ")))
                      (delete-region
                       (point)
                       (progn
                         (skip-chars-backward (lispy-comment-char 1 " \n"))
                         (point)))
                      (insert (concat " " (lispy-comment-char 2 " "))))
                     (t
                      (self-insert-command 1))))
              ((memq (char-before) '(?\\ ?\#))
               (self-insert-command 1))
              ((lispy-left-p)
               (setq bnd (lispy--bounds-dwim))
               (when lispy-move-after-commenting
                 (lispy-down 1))
               (comment-region (car bnd) (cdr bnd))
               (when lispy-move-after-commenting
                 (when (or (lispy--in-string-or-comment-p)
                           (looking-at (lispy-comment-char)))
                   (lispy--out-backward 1))))
              ((lispy-right-p)
               (if lispy-comment-use-single-semicolon
                   (progn
                     (unless (eolp)
                       (newline-and-indent)
                       (skip-chars-backward "\n\t "))
                     (comment-dwim nil)
                     (just-one-space))
                 (progn
                   (newline-and-indent)
                   (insert (lispy-comment-char 2 " "))
                   (unless (eolp)
                     (newline)
                     (lispy--reindent 1)
                     (skip-chars-backward "\n\t ")
                     (forward-char 1)))))
              ((eolp)
               (comment-dwim nil)
               (when lispy-comment-use-single-semicolon
                 (just-one-space)))
              ((looking-at " *[])}]")
               (if lispy-comment-use-single-semicolon
                   (if (lispy-bolp)
                       (insert (lispy-comment-char 2 "\n"))
                     (insert (lispy-comment-char 1 "\n")))
                 (progn
                   (unless (lispy-bolp)
                     (insert "\n"))
                   (insert (lispy-comment-char 2 "\n"))))
               (when (lispy--out-forward 1)
                 (lispy--prettify-1))
               (move-end-of-line 0)
               (insert " "))
              ((lispy-bolp)
               (let ((bnd (lispy--bounds-list)))
                 (cond ((null bnd)
                        (comment-region (point) (line-end-position)))
                       ((<= (cdr bnd) (line-end-position))
                        (comment-region (point)
                                        (1- (cdr bnd))))
                       (t
                        (let ((beg (point))
                              (ln-start (line-number-at-pos)))
                          (forward-sexp)
                          (while (and (= (line-number-at-pos) ln-start)
                                      (not (eolp)))
                            (forward-sexp))
                          (comment-region beg (point))
                          (goto-char beg))))
                 (skip-chars-forward " ")))
              ((setq bnd (save-excursion
                           (and (lispy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (if (= (count-matches lispy-left pt (point))
                            (count-matches lispy-right pt (point)))
                         (progn (comment-region pt (point))
                                (lispy-forward 1)
                                (lispy-backward 1))
                       (goto-char pt)
                       (re-search-forward lispy-left bnd t)
                       (backward-char 1)
                       (forward-list 1)
                       (comment-region pt (point))
                       (lispy-forward 1)
                       (lispy-backward 1))
                   (comment-region (point) (1- bnd))
                   (lispy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

(defun lispy--quote-string (str &optional quote-newlines)
  "Quote the quotes and backslashes in STR.
Quote the newlines if QUOTE-NEWLINES is t."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
  (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
  (if quote-newlines
      (replace-regexp-in-string "\n" "\\\\n" str)
    str))

(defun lispy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((bnd (lispy--bounds-dwim))
         (pt (point))
         (str-1 (buffer-substring-no-properties (car bnd) pt))
         (str-2 (buffer-substring-no-properties pt (cdr bnd)))
         (regionp (region-active-p))
         (leftp (lispy--leftp))
         deactivate-mark)
    (when (and regionp leftp)
      (exchange-point-and-mark))
    (if (lispy--in-string-p)
        (if regionp
            (progn
              (insert "\\\"")
              (exchange-point-and-mark)
              (insert "\\\"")
              (backward-char 2)
              (unless leftp
                (exchange-point-and-mark)))
          (lispy--complain "can't do anything useful here"))
      (deactivate-mark)
      (setq str-1 (lispy--quote-string str-1 (/= arg 1)))
      (setq str-2 (lispy--quote-string str-2 (/= arg 1)))
      (delete-region (car bnd) (cdr bnd))
      (insert "\"" str-1)
      (save-excursion (insert str-2 "\""))
      (when regionp
        (unless (looking-at "\"")
          (backward-char 1))
        (lispy-mark-symbol)
        (if (and leftp (= (point) (region-end)))
            (exchange-point-and-mark))))))

(defun lispy-unstringify ()
  "Unquote string at point."
  (interactive)
  (if (region-active-p)
      (if (lispy--string-markedp)
          (let (deactivate-mark
                (str (lispy--string-dwim))
                (leftp (lispy--leftp)))
            (delete-active-region)
            (set-mark (point))
            (insert (read str))
            (when leftp
              (lispy-other)))
        (lispy--complain "the current region isn't a string"))
    (let* ((bnd (lispy--bounds-string))
           (str (lispy--string-dwim bnd))
           (str-1 (concat (substring str 0 (- (point) (car bnd))) "\""))
           (offset (length (read str-1))))
      (delete-region (car bnd) (cdr bnd))
      (save-excursion (insert (read str)))
      (forward-char offset))))

(defvar lispy-teleport-global nil
  "When non-nil, `lispy-teleport' will consider all open parens in window.
Otherwise, only parens within the current defun are considered.
When you press \"t\" in `lispy-teleport', this will be bound to t temporarily.")

(defmacro lispy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun lispy-teleport (arg)
  "Move ARG sexps into a sexp determined by `lispy-ace-paren'."
  (interactive "p")
  (let ((beg (save-excursion
               (skip-chars-backward "'")
               (point)))
        end endp regionp
        deactivate-mark)
    (cond ((region-active-p)
           (if (= (point) (region-end))
               (progn
                 (setq end (region-beginning))
                 (setq endp t))
             (setq end (region-end)))
           (setq regionp t))
          ((lispy-left-p)
           (save-excursion
             (unless (lispy-dotimes arg
                       (forward-list 1))
               (error "Unexpected"))
             (setq end (point))))
          ((lispy-right-p)
           (save-excursion
             (setq endp t)
             (unless (lispy-dotimes arg
                       (backward-list arg))
               (error "Unexpected"))
             (setq end (point))))
          (t
           (error "Unexpected")))
    (let* ((lispy-avy-keys (delete ?t lispy-avy-keys))
           (avy-handler-function
            (lambda (x)
              (if (eq x ?t)
                  (progn
                    (avy--done)
                    (lispy-quit-and-run
                     (let ((lispy-teleport-global t))
                       (when regionp
                         (activate-mark))
                       (lispy-teleport arg))))
                (avy-handler-default x))))
           (res (lispy-ace-paren
                 (when lispy-teleport-global
                   2))))
      (cond ((memq res '(t nil))
             (when regionp
               (lispy--mark (cons end beg))))
            (t
             (forward-char 1)
             (unless (looking-at "(")
               (ignore-errors
                 (forward-sexp)))
             (backward-char 1)
             (lispy--teleport beg end endp regionp))))))

;;* Locals: dialect-related
(defcustom lispy-eval-display-style 'message
  "Choose a function to display the eval result."
  :type '(choice
          (const :tag "message" message)
          (const :tag "overlay" overlay)))

(declare-function cider--display-interactive-eval-result "ext:cider-overlays")
(declare-function eros--eval-overlay "ext:eros")

(define-error 'eval-error "Eval error")

(defvar lispy-message-limit 4000
  "String length limit for `lispy-message' to pop up a window.
For smaller strings `message' is used.")

(defun lispy-message (str &optional popup)
  "Display STR in the echo area.
If STR is too large, pop it to a buffer instead."
  (if (or
       popup
       (> (length str) lispy-message-limit)
       (> (cl-count ?\n str)
          (or
           14
           (* (window-height (frame-root-window)) max-mini-window-height))))
      (with-current-buffer (pop-to-buffer "*lispy-message*")
        (special-mode)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert str)
          (ignore-errors (pp-buffer))
          (goto-char (point-min)))
        str)
    (condition-case nil
        (message str)
      (error (message (replace-regexp-in-string "%" "%%" str))))))

(defvar lispy--pams (make-hash-table))

(defun lispy-pam-store (sym)
  "Store point and mark to SYM."
  (if (region-active-p)
      (progn
        (puthash sym (cons (point) (mark)) lispy--pams)
        (deactivate-mark))
    (puthash sym (point) lispy--pams)))

(defun lispy-pam-restore (sym)
  "Restore point and mark from FROM."
  (let ((val (gethash sym lispy--pams)))
    (cond ((consp val)
           (goto-char (car val))
           (set-mark (cdr val)))
          ((numberp val)
           (goto-char val)))))

(defun lispy-beginning-of-defun (&optional arg)
  "Forward to `beginning-of-defun' with ARG.  Deactivate region.
When called twice in a row, restore point and mark."
  (interactive "p")
  (cond ((and (called-interactively-p 'any)
              (looking-at "^(")
              (let* ((lispy-bof-last-point (gethash 'lispy-bof-last-point lispy--pams))
                     (pt (if (consp lispy-bof-last-point)
                             (car lispy-bof-last-point)
                           lispy-bof-last-point)))
                (and
                 (> pt (point))
                 (<= pt (save-excursion (forward-list) (point))))))
         (lispy-pam-restore 'lispy-bof-last-point))
        ((looking-at "^("))
        (t
         (lispy-pam-store 'lispy-bof-last-point)
         (beginning-of-defun arg))))

;;* Locals: avy-jump
(declare-function avy--regex-candidates "avy")
(declare-function avy-process "avy")
(declare-function avy--overlay-post "avy")

(defun lispy-ace-char ()
  "Visually select a char within the current defun."
  (interactive)
  (let ((avy-keys lispy-avy-keys))
    (avy-with lispy-ace-char
      (lispy--avy-do
       (string (read-char "Char: "))
       (save-excursion
         ;; `beginning-of-defun' won't work, since it can change sexp
         (lispy--out-backward 50)
         (lispy--bounds-dwim))
       (lambda () t)
       lispy-avy-style-char))))

(defun lispy-ace-paren (&optional arg)
  "Jump to an open paren within the current defun.
ARG can extend the bounds beyond the current defun."
  (interactive "p")
  (setq arg (or arg 1))
  (lispy--remember)
  (deactivate-mark)
  (let ((avy-keys lispy-avy-keys)
        (bnd (if (eq arg 1)
                 (save-excursion
                   (lispy--out-backward 50)
                   (lispy--bounds-dwim))
               (cons (window-start)
                     (window-end nil t)))))
    (avy-with lispy-ace-paren
      (lispy--avy-do
       lispy-left
       bnd
       (lambda () (not (lispy--in-string-or-comment-p)))
       lispy-avy-style-paren))))

(defun lispy-ace-symbol (arg)
  "Jump to a symbol within the current sexp and mark it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (let ((avy-keys lispy-avy-keys)
        res)
    (avy-with lispy-ace-symbol
      (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) -1 0)))
        (setq res (lispy--avy-do
                   "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
                   (lispy--bounds-dwim)
                   (lambda ()
                     (not (save-excursion
                            (forward-char -1)
                            (lispy--in-string-or-comment-p))))
                   lispy-avy-style-symbol))))
    (unless (memq res '(t nil))
      (unless (or (eq (char-after) ?\")
                  (looking-at ". "))
        (forward-char 1))
      (lispy-mark-symbol))))

(defun lispy-ace-symbol-beginning-of-defun ()
  (interactive)
  (lispy-ace-symbol 99))

(defun lispy-ace-subword (arg)
  "Mark sub-word within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (if (and (region-active-p)
           (string-match "\\`\\(\\sw+\\)\\s_"
                         (lispy--string-dwim)))
      (lispy--mark (cons (region-beginning)
                         (+ (region-beginning) (match-end 1))))
    (lispy--out-forward
     (if (region-active-p)
         (progn (deactivate-mark) arg)
       (1- arg)))
    (let* ((avy-keys lispy-avy-keys)
           (res (avy-with 'lispy-ace-subword
                  (lispy--avy-do
                   "[([{ -/]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
                   (lispy--bounds-dwim)
                   (lambda () (or (not (lispy--in-string-or-comment-p))
                                  (lispy-looking-back ".\"")))
                   lispy-avy-style-symbol))))
      (unless (memq res '(t nil))
        (skip-chars-forward "-([{ `'#")
        (mark-word)))))

(defun lispy--avy-do (regex bnd filter style &optional group)
  "Visually select a match to REGEX within BND.
Filter out the matches that don't match FILTER.
Use STYLE function to update the overlays."
  (lispy--recenter-bounds bnd)
  (let* ((avy-all-windows nil)
         (cands (avy--regex-candidates
                 regex
                 (car bnd) (cdr bnd)
                 filter
                 group)))
    (dolist (x cands)
      (when (> (- (cdar x) (caar x)) 1)
        (cl-incf (caar x))))
    (avy-process
     cands
     (cl-case style
       (pre #'avy--overlay-pre)
       (at #'avy--overlay-at)
       (at-full #'avy--overlay-at-full)
       (post #'avy--overlay-post)))))

(declare-function ediff-regions-internal "ediff")

(defun lispy-tab ()
  "Indent code.
When region is active, call `lispy-mark-car'."
  (interactive)
  (if (region-active-p)
      (lispy-mark-car)
    (lispy--prettify-1)))

;;* Locals: marking
(defun lispy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (let* ((pt (point))
         (mk (mark))
         (lispy-ignore-whitespace t)
         (r (lispy--out-forward arg)))
    (deactivate-mark)
    (if (or (= pt (point))
            (= mk (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (lispy--complain "can't go any further")
          (if (> mk pt)
              (lispy--mark (cons pt mk))
            (lispy--mark (cons mk pt)))
          nil)
      (lispy--mark
       (lispy--bounds-dwim))
      r)))

(defun lispy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (if (lispy-mark-right arg)
      (lispy-other)
    (when (= (point) (region-end))
      (exchange-point-and-mark))))

(defun lispy-mark-car ()
  "Mark the car of current thing."
  (interactive)
  (lispy--remember)
  (let ((bnd-1 (lispy--bounds-dwim))
        bnd-2)
    (cond ((and (eq (char-after (car bnd-1)) ?\")
                (eq (char-before (cdr bnd-1)) ?\")
                (eq 1 (length (read (format "(%s)" (lispy--string-dwim))))))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((and (eq (char-after (car bnd-1)) ?\`)
                (eq (char-before (cdr bnd-1)) ?\'))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((save-excursion
             (goto-char (car bnd-1))
             (looking-at "\\(['`,@]+\\)\\w"))
           (set-mark (match-end 1))
           (goto-char (cdr bnd-1)))

          ((and (region-active-p)
                (or (and (= (point) (region-end))
                         (looking-at "\\_>"))
                    (and (= (point) (region-beginning))
                         (looking-at "\\_<")))))
          (t
           (goto-char (car bnd-1))
           (while (and (equal bnd-1 (setq bnd-2 (bounds-of-thing-at-point 'sexp)))
                       (< (point) (cdr bnd-1)))
             (forward-char)
             (skip-chars-forward " "))
           (if bnd-2
               (lispy--mark bnd-2)
             (lispy--complain "can't descend further"))))))

;;* Locals: miscellanea
(declare-function lispy--eval-python "le-python")

(defun lispy-undo ()
  "Deactivate region and `undo'."
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (undo))

(unless (fboundp 'macrop)
  (defun macrop (object)
    "Non-nil if and only if OBJECT is a macro."
    (let ((def (indirect-function object)))
      (when (consp def)
        (or (eq 'macro (car def))
            (and (autoloadp def) (memq (nth 4 def) '(macro t))))))))

(defalias 'lispy--preceding-sexp
    (if (fboundp 'elisp--preceding-sexp)
        'elisp--preceding-sexp
      'preceding-sexp))

(defun lispy-narrow (arg)
  "Narrow ARG sexps or region."
  (interactive "p")
  (cond ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((lispy-left-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-forward arg)
                             (point))))
        ((lispy-right-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-backward arg)
                             (point))))))

(defun lispy-widen ()
  "Forward to `widen'."
  (interactive)
  (widen))

(defun lispy-paste (arg)
  "Forward to `yank'.
If the region is active, replace instead of yanking.
When ARG is given, paste at that place in the current list."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim)))
           (deactivate-mark)
           (lispy--maybe-safe-delete-region (car bnd)
                                            (cdr bnd))
           (insert (lispy--maybe-safe-current-kill))))
        ((> arg 1)
         (lispy-mark-car)
         (lispy-down (- arg 2))
         (deactivate-mark)
         (just-one-space)
         (insert (lispy--maybe-safe-current-kill))
         (unless (or (eolp) (looking-at lispy-right))
           (just-one-space)
           (forward-char -1)))
        ((lispy-right-p)
         (newline-and-indent)
         (insert (lispy--maybe-safe-current-kill)))
        ((lispy-left-p)
         (newline-and-indent)
         (forward-line -1)
         (lispy--indent-for-tab)
         (insert (lispy--maybe-safe-current-kill)))
        (t
         (insert (lispy--maybe-safe-current-kill)))))

(defalias 'lispy-font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      'font-lock-ensure
    'font-lock-fontify-buffer))

(defun lispy-map-done ()
  (interactive)
  (lispy-map-delete-overlay)
  (setq lispy-bind-var-in-progress nil)
  (lispy-backward 1))

(defun lispy-map-delete-overlay ()
  "Delete `lispy-map-input-overlay'."
  (when (overlayp lispy-map-input-overlay)
    (delete-overlay lispy-map-input-overlay)))

;;* Predicates
(defun lispy--in-string-p ()
  "Test if point is inside a string.
Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defun lispy--in-comment-p ()
  "Test if point is inside a comment."
  (or
   (save-excursion
     (unless (eolp)
       (forward-char 1))
     (nth 4 (syntax-ppss)))
   (and (bolp) (looking-at lispy-outline-header))))

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun lispy--raw-comment-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'comment)))

(defun lispy--raw-string-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'string)))

(defun lispy--leftp ()
  "Return t if at region beginning, or at start of the list."
  (if (region-active-p)
      (= (point) (region-beginning))
    (or (lispy-left-p)
        (looking-at lispy-outline))))

(defun lispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun lispy--string-markedp ()
  "Return t if the current active region is a string."
  (and (region-active-p)
       (eq ?\" (char-after (region-beginning)))
       (eq ?\" (char-before (region-end)))))

(defun lispy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun lispy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun lispy--empty-line-p ()
  "Test whether the point is on an \"empty\" line.
Return t if the point is by itself on a line with optional whitespace.
Return 'right if the point is on a line with only right delimiters and
whitespace."
  (if (and (looking-at (concat "[[:space:]]*" lispy-right "*$"))
           (lispy-looking-back "^[[:space:]]*"))
      (if (looking-at (concat "[[:space:]]*" lispy-right))
          'right
        t)
    nil))

(defun lispy--preceding-syntax (preceding-syntax-alist &optional before after)
  "Return a regexp corresponding to valid syntax that can precede delimiters.
This is done by checking PRECEDING-SYNTAX-ALIST for the current major mode.
Return nil if there is no entry for the current major mode. When there is an
entry, prepend BEFORE and append AFTER to the regexp when they are specified."
  (let ((regexps (or (cdr (assoc major-mode preceding-syntax-alist))
                     (cdr (assoc t preceding-syntax-alist)))))
    (when regexps
      (concat before
              "\\(?:"
              (apply #'concat
                     (lispy-interleave
                      "\\|"
                      regexps))
              "\\)"
              after))))

(defun lispy--in-empty-list-p (preceding-syntax-alist)
  "Test whether the point is in a list with no sexps.
A list with only characters that can precede a delimiter (e.g. \"`(,)\") is
consider an empty list."
  (and (lispy-looking-back
        (concat lispy-left
                "[[:space:]]*"
                (lispy--preceding-syntax preceding-syntax-alist nil "*")))
       (looking-at (concat "[[:space:]]*" lispy-right))))

(defun lispy--not-at-sexp-p (preceding-syntax-alist)
  "Test whether the point is at a \"free\" spot and not at a wrappable sexp.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede an opening delimiter in
each major mode."
  (let* ((space "[[:space:]]")
         (space-or-eol (concat "\\(" space "+\\|" space "*$\\)"))
         (right-or-eol (concat "\\(" lispy-right "+\\|" space "*$\\)"))
         (special-syntax (lispy--preceding-syntax preceding-syntax-alist))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (or (lispy--in-empty-list-p preceding-syntax-alist)
        ;; empty line
        (string-match (concat "^" space "*" special-syntax "*" space "*$")
                      line)
        ;; empty position at end of list or line
        (and (looking-at right-or-eol)
             (lispy-looking-back (concat space "+" special-syntax "*")))
        ;; empty position at beginning of list
        (and (looking-at space-or-eol)
             (lispy-looking-back (concat lispy-left special-syntax "*")))
        ;; empty position in middle
        (and (looking-at (concat space "+"))
             (lispy-looking-back (concat space "+" special-syntax "*"))))))

;;* Pure
(declare-function lispy-bounds-python-block "le-python")

(defun lispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (let (bnd)
    (cond ((region-active-p)
           (cons (region-beginning)
                 (region-end)))
          ((and (setq bnd (lispy--bounds-string))
                (or (eq (point) (car bnd))
                    (eq (point) (1- (cdr bnd)))))
           bnd)
          ((looking-at lispy-outline)
           (save-excursion
             (cons
              (progn
                (outline-end-of-heading)
                (1+ (point)))
              (progn
                (outline-end-of-subtree)
                (skip-chars-backward "\n")
                (when (setq bnd (lispy--bounds-comment))
                  (goto-char (1- (car bnd))))
                (point)))))
          ((save-excursion
             (when (lispy-right-p)
               (backward-list))
             (and (or (looking-at (concat "[^[:space:]\n]*" lispy-left))
                      (looking-at "[`'#]"))
                  (setq bnd (bounds-of-thing-at-point 'sexp))))
           (save-excursion
             (goto-char (car bnd))
             (lispy--skip-delimiter-preceding-syntax-backward)
             (cons (point) (cdr bnd))))
          ((looking-at (lispy-comment-char 2))
           (lispy--bounds-comment))
          ((and (eq major-mode 'python-mode)
                (lispy-bolp))
           (lispy-bounds-python-block))
          (t
           (let ((res (ignore-errors
                        (bounds-of-thing-at-point
                         (if (looking-at lispy-right)
                             'symbol
                           'sexp)))))
             (if res
                 (save-excursion
                   (goto-char (cdr res))
                   (lispy--in-string-or-comment-p)
                   (skip-chars-backward "[.,]")
                   (cons (car res) (point)))
               (or
                (ignore-errors
                  (bounds-of-thing-at-point 'symbol))
                (and (lispy-looking-back "\" *")
                     (save-excursion
                       (goto-char (match-beginning 0))
                       (lispy--bounds-string)))
                (ignore-errors
                  (bounds-of-thing-at-point 'sentence))
                (ignore-errors
                  (save-excursion
                    (backward-word 1)
                    (bounds-of-thing-at-point 'symbol)))
                (ignore-errors
                  (save-excursion
                    (forward-word 1)
                    (bounds-of-thing-at-point 'symbol))))))))))

(declare-function python-nav-end-of-statement "python")

(defun lispy--bounds-list ()
  "Return the bounds of smallest list that includes the point."
  (save-excursion
    (lispy--exit-string)
    (when (looking-at lispy-left)
      (forward-char))
    (when (lispy-looking-back lispy-right)
      (backward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))

(defun lispy--bounds-string ()
  "Return bounds of current string."
  (unless (lispy--in-comment-p)
    (let ((beg (or (nth 8 (syntax-ppss))
                   (and (eq (char-after (point)) ?\")
                        (not (eq ?\\ (char-before)))
                        (point)))))
      (when (and beg (not (comment-only-p beg (1+ (point)))))
        (ignore-errors
          (cons beg (save-excursion
                      (goto-char beg)
                      (forward-sexp)
                      (point))))))))

(defun lispy--bounds-comment ()
  "Return bounds of current comment."
  (and (lispy--in-comment-p)
       (save-excursion
         (when (lispy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (lispy--in-comment-p)
                         (forward-comment -1)
                         (lispy-looking-back "^[[:space:]]*")
                         (= 1 (- (count-lines (point) pt)
                                 (if (bolp) 0 1))))
               (setq pt (point)))
             (goto-char pt))
           (if (looking-at "#|")
               (cons (point)
                     (progn
                       (comment-forward)
                       (point)))
             (let ((beg (lispy--beginning-of-comment))
                   (pt (point))
                   (col (current-column)))
               (while (and (lispy--in-comment-p)
                           (forward-comment 1)
                           (lispy--beginning-of-comment)
                           (and (= 1 (- (count-lines pt (point))
                                        (if (bolp) 0 1)))
                                ;; count comments starting in different columns
                                ;; as separate
                                (= col (current-column))
                                ;; if there's code in between,
                                ;; count comments as separate
                                (lispy-looking-back "^\\s-*")))
                 (setq pt (point)))
               (goto-char pt)
               (end-of-line)
               (cons beg (point))))))))

(defun lispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lispy--bounds-dwim' is used if BOUNDS is nil."
  (setq bounds (or bounds (lispy--bounds-dwim)))
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(declare-function python-info-current-symbol "python")

(defun lispy--current-function ()
  "Return current function as string."
  (if (region-active-p)
      (let ((str (lispy--string-dwim)))
        (if (string-match "\\`[#'`]*\\(.*?\\)'?\\'" str)
            (match-string 1 str)
          nil))
    (save-excursion
      (if (eq major-mode 'python-mode)
          (let ((bnd (bounds-of-thing-at-point 'symbol)))
            (if bnd
                (lispy--string-dwim bnd)
              (up-list -1)
              (python-info-current-symbol)))
        (lispy--back-to-paren)
        (when (looking-at "(\\([^ \n)]+\\)[ )\n]")
          (match-string-no-properties 1))))))

;;* Utilities: movement
(defun lispy--out-forward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (lispy--exit-string)
  (catch 'break
    (dotimes (_i arg)
      (if (ignore-errors (up-list) t)
          (if buffer-read-only
              (deactivate-mark)
            (unless (or ignore-ws lispy-ignore-whitespace)
              (lispy--remove-gaps)
              (lispy--indent-for-tab)))
        (when (lispy-left-p)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun lispy--out-backward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((oldpt (point))
        newpt)
    (lispy--out-forward arg ignore-ws)
    (when (lispy-right-p)
      (forward-list -1))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

(defun lispy--back-to-paren ()
  "Move to ( going out backwards."
  (let ((lispy-ignore-whitespace t))
    (lispy--exit-string)
    (while (and (not (looking-at "("))
                (lispy--out-backward 1)))))

(defun lispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun lispy--beginning-of-comment ()
  "Go to beginning of comment on current line."
  (end-of-line)
  (comment-beginning)
  (let ((cs (comment-search-backward (line-beginning-position) t)))
    (or
     (when cs
       (goto-char cs))
     (and (looking-at (concat "^" lispy-outline-header))
          (point)))))

(defun lispy--skip-delimiter-preceding-syntax-backward ()
  "Move backwards past syntax that could precede an opening delimiter such as '.
Specifically, move backwards to the closest whitespace char or opening delimiter
or to the beginning of the line."
  (re-search-backward (concat "[[:space:]]" "\\|"
                              lispy-left "\\|"
                              "^"))
  (goto-char (match-end 0)))

;;* Utilities: slurping and barfing
(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (skip-chars-forward " \t")
    (delete-region pt (point))
    (unless (or (lispy-after-string-p "()")
                (lispy-after-string-p "[]")
                (lispy-after-string-p "{}")
                (eolp))
      (insert " "))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert char))))

(defun lispy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert char)
    (backward-char)))

(defun lispy--barf-forward ()
  "Shrink current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (unless (looking-at "()")
      (forward-char)
      (forward-sexp)
      (delete-region pt (1+ pt))
      (skip-chars-forward " \n	")
      (insert char)
      (backward-char)
      (indent-region pt (point))
      (lispy--reindent 1))))

(defun lispy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (unless (lispy-after-string-p "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (lispy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert char)
      (lispy--indent-region (point) pt))))

(defun lispy--replace-regexp-in-code (regexp to-string)
  "Replace text matching REGEXP with TO-STRING in whole buffer.
Ignore the matches in strings and comments."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (lispy--in-string-or-comment-p)
      (replace-match to-string))))

;;* Utilities: source transformation
(defvar lispy--braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for paired braces.")

(defvar lispy--insert-replace-alist-clojure
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("@(" "clojure-deref-list")
    ("#(" "clojure-lambda")
    ("#{" "clojure-set")
    ("@{" "clojure-deref-map")
    ("@[" "clojure-deref-vector")
    ("{" "clojure-map")
    ("#?(" "clojure-reader-conditional")))

(defvar lispy--insert-replace-alist-elisp
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("#(" "clojure-lambda")
    ("#?(" "clojure-reader-conditional")))

(defun lispy--read-1 ()
  (let* ((alist (if (member major-mode lispy-elisp-modes)
                    lispy--insert-replace-alist-elisp
                  lispy--insert-replace-alist-clojure))
         (regex (regexp-opt (mapcar #'car alist))))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let* ((head-beg (match-beginning 0))
             (head-end (match-end 0))
             (head (match-string 0))
             (entry (assoc head alist))
             (class (cadr entry))
             str-mid)
        (unless (lispy--in-string-or-comment-p)
          (backward-char 1)
          (save-excursion
            (if (save-match-data
                  (looking-at "((ly-raw string"))
                (forward-list 1)
              (with-syntax-table lispy--braces-table
                (forward-list 1)))
            (setq str-mid (buffer-substring-no-properties head-end (1- (point))))
            (delete-region head-beg (point)))
          (insert "(ly-raw " class " (" str-mid "))")
          (backward-char (+ 3 (length str-mid))))))))

(defvar lispy--clojure-char-literal-regex
  (format "\\\\\\(\\(?:\\(?:\\sw\\|%s\\)\\b\\)\\|[.,!@#$%%&*]\\|u[A-Za-z0-9]+\\)"
          (regexp-opt '("newline" "space" "tab" "formfeed" "backspace" "return")))
  "Regex for Clojure character literals.
See https://clojure.org/guides/weird_characters#_character_literal.")

(defun lispy--read-replace (regex class &optional subexp)
  (setq subexp (or subexp 0))
  (goto-char (point-min))
  (while (re-search-forward regex nil t)
    (cond ((string= (match-string 0) "ly-raw")
           (if (looking-at " clojure-\\(map\\|set\\|lambda\\)")
               (goto-char (match-end 0))
             (up-list)))
          ((lispy--in-string-or-comment-p))
          (t
           (replace-match
            (format "(ly-raw %s %S)"
                    class
                    (substring-no-properties
                     (match-string subexp)))
            t t nil subexp)))))

;; TODO: Make the read test pass on string with semi-colon
(defun lispy--read (str)
  "Read STR including comments and newlines."
  (let* ((deactivate-mark nil)
         (mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— ly-raw —————————————————
                (lispy--replace-regexp-in-code "(ly-raw" "(ly-raw raw")
                ;; ——— comments ———————————————
                (goto-char (point-min))
                (while (comment-search-forward (point-max) t)
                  (lispy--beginning-of-comment)
                  (setq cbnd (cons (point) (line-end-position)))
                  (setq str (lispy--string-dwim cbnd))
                  (delete-region (car cbnd) (cdr cbnd))
                  (insert (format "(ly-raw comment %S)" str)))
                ;; ——— reader macro syntax (LISP)
                (goto-char (point-min))
                (while (re-search-forward "#[a-z][\"(]" nil t)
                  (forward-char -1)
                  (unless (lispy--in-string-or-comment-p)
                    (let ((beg (match-beginning 0))
                          rep)
                      (forward-sexp 1)
                      (setq rep (format "(ly-raw lisp-macro %S)"
                                        (buffer-substring-no-properties
                                         beg (point))))
                      (delete-region beg (point))
                      (insert rep))))
                ;; ——— strings ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\"" nil t)
                  (progn
                    (setq cbnd (lispy--bounds-string))
                    (when cbnd
                      (if (or (lispy-after-string-p "ly-raw comment \"")
                              (lispy-after-string-p "ly-raw lisp-macro \""))
                          (goto-char (cdr cbnd))
                        (setq str (lispy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (lispy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— numbers ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\b[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]*\\)" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (let ((s (match-string-no-properties 0)))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert (format "(ly-raw float \"%s\")" s)))))
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)\\(()\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)" nil nil nil 1)))
                ;; ——— \ char syntax (Clojure)—
                (when (eq major-mode 'clojure-mode)
                  (lispy--read-replace lispy--clojure-char-literal-regex "clojure-char"))
                ;; namespaced map #520
                (when (memq major-mode lispy-clojure-modes)
                  (goto-char (point-min))
                  (while (re-search-forward "#\\(?:js\\|:\\(?:\\sw\\|\\s_\\)+\\) *\\(?:{\\|\\[\\)" nil t)
                    (let* ((head-beg (match-beginning 0))
                           (head-end (match-end 0))
                           (head (match-string 0))
                           str-mid tail)
                      (unless (lispy--in-string-or-comment-p)
                        (backward-char 1)
                        (save-excursion
                          (with-syntax-table lispy--braces-table
                            (forward-list 1))
                          (setq str-mid (buffer-substring-no-properties head-end (1- (point))))
                          (setq tail (buffer-substring-no-properties (1- (point)) (point)))
                          (delete-region head-beg (point)))
                        (insert
                         (format
                          "(ly-raw splice \"%s\" \"%s\" (%s))"
                          (replace-regexp-in-string " +" "" (substring-no-properties head))
                          tail
                          str-mid))
                        (backward-char (+ 3 (length str-mid)))))))
                ;; ——— #{ or { or #( or @( or #?( or #?@( ——————————
                (lispy--read-1)
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (if (memq major-mode (cons 'hy-mode lispy-clojure-modes))
                    (lispy--read-replace "[[:alnum:]-/*<>_?.,\\\\:!@#=]+" "clojure-symbol")
                  (while (re-search-forward "\\(?:\\s-\\|\\s(\\)\\?" nil t)
                    (unless (lispy--in-string-or-comment-p)
                      (let ((pt (point))
                            sexp)
                        (lispy--skip-elisp-char)
                        (setq sexp (buffer-substring-no-properties pt (point)))
                        (delete-region (1- pt) (point))
                        (insert (format "(ly-raw char %S)" sexp))))))
                (when (eq major-mode 'clojure-mode)
                  (lispy--read-replace " *,+" "clojure-commas"))
                ;; ——— \ char syntax (LISP)————
                (goto-char (point-min))
                (while (re-search-forward "#\\\\\\(.\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw lisp-char %S)"
                                           (substring-no-properties
                                            (match-string 0)))
                                   nil t)))
                ;; ——— Clojure gensym —————————
                (goto-char (point-min))
                (while (re-search-forward "\\([a-zA-Z][a-zA-z-/_0-9]*#\\)[ \t\n\r]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match
                     (format "(ly-raw clojure-gensym %S)"
                             (match-string-no-properties 1))
                     t nil nil 1)))
                ;; ——— Clojure keyword —————————
                (goto-char (point-min))
                (while (re-search-forward "\\(:\\.[^][({}) \t\n\r\"]+\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw clojure-keyword %S)"
                                           (match-string-no-properties 1)))))
                ;; ——— #' —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (forward-sexp)
                    (insert ")")
                    (replace-match "(ly-raw function ")))
                ;; ——— ,@ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\),@" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\,@))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 2)
                        (insert "(ly-raw comma-splice ")))))
                ;; ——— #_ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#_[({[]" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (backward-char 1)
                    (let ((beg (point)))
                      (forward-list 1)
                      (insert ")")
                      (goto-char beg)
                      (delete-char -2)
                      (insert "(ly-raw clojure-reader-comment "))))
                ;; ——— #1 —————————————————————
                ;; Elisp syntax for circular lists
                (goto-char (point-min))
                (while (re-search-forward "\\(?:^\\|\\s-\\|\\s(\\)\\(#[0-9]+\\)" nil t)
                  (unless (lispy--in-string-p)
                    (replace-match (format "(ly-raw reference %S)"
                                           (substring-no-properties
                                            (match-string 1)))
                                   nil nil nil 1)))
                ;; ——— ' ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) 'quote))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw quote ")))))
                ;; ——— ` ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)`" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (cond ((looking-at lispy-left)
                           (delete-char -1)
                           (insert "(ly-raw \\` ")
                           (forward-list 1)
                           (insert ")")
                           (backward-list 1)
                           (forward-char 7))
                          ((looking-at "\\sw\\|\\s_\\|[,@]")
                           (let ((beg (point)))
                             (forward-sexp 1)
                             (insert "\")")
                             (goto-char (1- beg))
                             (insert "(ly-raw quasiquote \""))))))
                ;; ——— , ——————————————————————
                (lispy--replace-regexp-in-code "\\\\," "(ly-raw comma-symbol)")
                (goto-char (point-min))
                (while (re-search-forward "[^\\]?,[^@\"]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (if (memq major-mode lispy-clojure-modes)
                        (progn
                          (delete-char 1)
                          (insert "(ly-raw clojure-comma)"))
                      (let ((beg (point))
                            (sxp (ignore-errors (read (current-buffer)))))
                        (when (and (consp sxp)
                                   (eq (car sxp) '\,))
                          (insert ")")
                          (goto-char beg)
                          (delete-char 1)
                          (insert "(ly-raw \\, "))))))
                ;; ——— angle syntax —————————
                ;; used for markers/buffers/windows/overlays
                (goto-char (point-min))
                (while (re-search-forward "#<" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "(ly-raw angle \"")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert "\")")))
                ;; ——— cons cell syntax ———————
                (lispy--replace-regexp-in-code " \\. " " (ly-raw dot) ")
                ;; Racket stuff
                (lispy--replace-regexp-in-code "#t" "(ly-raw racket-true)")
                (lispy--replace-regexp-in-code "#f" "(ly-raw racket-false)")
                (goto-char (point-min))
                (while (re-search-forward "#:\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw racket-option %s)"
                                           (match-string 1)))))
                ;; Clojure # in a symbol
                (goto-char (point-min))
                (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" nil t)
                  (unless (lispy--in-string-p)
                    (when (cl-position ?# (match-string 0))
                      (let* ((bnd (lispy--bounds-dwim))
                             (str (lispy--string-dwim bnd)))
                        (delete-region (car bnd) (cdr bnd))
                        (insert (format "(ly-raw symbol %S)" str))))))
                ;; Clojure (. object method)
                (goto-char (point-min))
                (while (re-search-forward "(\\.[\t\n ]" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (forward-char -1)
                    (delete-char -1)
                    (insert "(ly-raw clojure-dot)")))
                ;; ———  ———————————————————————
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
    (ignore-errors
      (read str))))

(defun lispy--skip-elisp-char ()
  (unless (lispy-after-string-p "?")
    (error "unexpected"))
  (if (looking-at "\\\\")
      (forward-sexp 1)
    (forward-char 1)))

(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar
                       (lambda (parent)
                         (cons parent
                               (or (get parent 'error-conditions)
                                   (error "Unknown signal `%s'" parent))))
                       parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(define-error 'unsupported-mode-error "Unsupported mode")
(defun lispy--replace (lst from to)
  "Recursively replace elements in LST from FROM to TO."
  (cond ((eq lst from)
         to)
        ((not (consp lst))
         lst)
        (t
         (cons
          (lispy--replace (car lst) from to)
          (lispy--replace (cdr lst) from to)))))

;;* Utilities: error reporting
(defun lispy--complain (msg)
  "Display MSG if `lispy-verbose' is t."
  (when (and lispy-verbose
             (null noninteractive))
    (message "[lispy] %s: %s"
             (prin1-to-string this-command)
             msg)))

(defun lispy--complain-not-supported ()
  (lispy--complain "Command not supported for current mode. Please consult the variable `lispy--handlers-alist`."))

(defun lispy--complain-missing-eval-handler ()
  (lispy--complain "Could not find eval handler for current mode."))

(defun lispy--complain-unrecognized-key ()
  (lispy--complain "Ignoring unmapped key."))

;;* Utilities: rest
(defun lispy--indent-region (beg end)
  "Indent region BEG END without reporting progress."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1))
    (move-marker end nil)))

(defvar lispy-no-indent-modes '(minibuffer-inactive-mode
                                comint-mode)
  "List of major modes where `indent-for-tab-command' should not be used.
`lispy--indent-for-tab' will do nothing if the current mode or any of its parent
modes is in this list.")

(defun lispy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (unless (or (memq major-mode lispy-no-indent-modes)
              (apply #'derived-mode-p lispy-no-indent-modes)
              (= 0 (buffer-size)))
    (let ((tab-always-indent t))
      (lispy-flet (message (&rest _x))
        (indent-for-tab-command)))))

(defun lispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (and (lispy-right-p)
             (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)"
                           (condition-case nil
                               (save-excursion
                                 (backward-list)
                                 (point))
                             (error (point-min))))
             (not (eq ?\\ (aref (match-string 0) 0))))
    (unless (save-excursion
              (save-match-data
                (goto-char (match-beginning 1))
                (lispy--in-string-or-comment-p)))
      (delete-region (match-beginning 1)
                     (match-end 1)))))

(defun lispy--surround-region (alpha omega)
  "Surround active region with ALPHA and OMEGA and re-indent."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert omega)
    (goto-char beg)
    (insert alpha)
    (deactivate-mark)
    (indent-region beg (+ 2 end))))

(defun lispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun lispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (let ((inhibit-field-text-motion t))
    (unless (or lispy-no-space
                (bolp)
                (and (window-minibuffer-p)
                     (eq (point) (minibuffer-prompt-end)))
                (lispy--in-string-or-comment-p)
                (lispy-looking-back context))
      (insert " "))))

(defun lispy--delimiter-space-unless (preceding-syntax-alist)
  "Like `lispy--space-unless' but use PRECEDING-SYNTAX-ALIST for decision.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
When `looking-back' at any of these regexps, whitespace, or a delimiter, do not
insert a space."
  (lispy--space-unless
   (concat "^\\|\\s-\\|" lispy-left
           (lispy--preceding-syntax preceding-syntax-alist "\\|"))))

(defun lispy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond ((region-active-p)
         (indent-region (region-beginning)
                        (region-end)))
        (arg
         (lispy-save-excursion
           (lispy--out-forward arg)
           (backward-list)
           (indent-sexp)))

        ((lispy-right-p)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((lispy-left-p)
         (indent-sexp))

        (t
         (save-excursion
           (lispy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lispy--delete ()
  "Delete one sexp."
  (unless (lispy-left-p)
    (error "Bad position"))
  (let ((bnd (lispy--bounds-list)))
    (delete-region (car bnd) (cdr bnd))
    (cond ((looking-at (concat "\n+" lispy-left))
           (delete-region (match-beginning 0)
                          (1- (match-end 0))))
          ((looking-at "\n\n+"))
          ((looking-at "\\([ ]*\\)\n")
           (delete-region (match-beginning 1)
                          (match-end 1)))
          ((looking-at lispy-right))
          ((eolp))
          (t
           (just-one-space)
           (when (lispy-after-string-p "( ")
             (backward-delete-char 1))))))

(defun lispy--recenter-bounds (bnd)
  "Make sure BND is visible in window.
BND is a cons of start and end points."
  (cond ((> (count-lines (car bnd) (cdr bnd))
            (window-height)))
        ((< (car bnd) (window-start))
         (save-excursion
           (goto-char (car bnd))
           (recenter 0)))
        ((> (cdr bnd) (window-end))
         (save-excursion
           (goto-char (cdr bnd))
           (recenter -1)))))

(defun lispy--prin1-to-string (expr offset mode)
  "Return the string representation of EXPR.
EXPR is indented first, with OFFSET being the column position of
the first character of EXPR.
MODE is the major mode for indenting EXPR."
  (let ((lif lisp-indent-function))
    (with-temp-buffer
      (funcall mode)
      (dotimes (_i offset)
        (insert ?\ ))
      (let ((lisp-indent-function lif))
        (lispy--insert expr))
      (buffer-substring-no-properties
       (+ (point-min) offset)
       (point-max)))))

(defun lispy--splice-to-str (sexp)
  "Return the printed representation of SEXP.
The outer delimiters are stripped."
  (if sexp
      (substring
       (prin1-to-string sexp) 1 -1)
    ""))

(defun lispy--insert (expr)
  "Insert the EXPR read by `lispy--read'."
  (let ((start-pt (point))
        beg
        sxp type)
    (prin1 expr (current-buffer))
    (save-restriction
      (narrow-to-region start-pt (point))
      (goto-char (point-min))
      (while (and (re-search-forward "(ly-raw" nil t)
                  (setq beg (match-beginning 0)))
        (goto-char beg)
        (setq sxp (ignore-errors (read (current-buffer))))
        (setq type (cadr sxp))
        (cl-case type
          (newline
           (delete-region beg (point))
           (delete-char
            (- (skip-chars-backward " ")))
           (insert "\n"))
          ((string comment symbol float quasiquote)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (comma-symbol
           (delete-region beg (point))
           (insert "\\,"))
          (ignore
           (delete-region beg (point))
           (backward-delete-char 1))
          (raw
           (delete-region beg (point))
           (prin1 (cons 'ly-raw (cddr sxp))
                  (current-buffer))
           (backward-list)
           (forward-char 7))
          (quote
           (delete-region beg (point))
           (insert "'")
           (let ((it (cl-caddr sxp)))
             (if it
                 (prin1 it (current-buffer))
               (insert "()")))
           (goto-char beg))
          (empty
           (delete-region beg (point))
           (insert "()"))
          (char
           (delete-region beg (point))
           (insert "?" (cl-caddr sxp)))
          ((clojure-char)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          ((clojure-commas)
           (delete-region (1- beg) (point))
           (insert (cl-caddr sxp)))
          (clojure-symbol
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-char
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-macro
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          ((clojure-gensym clojure-keyword)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (function
           (delete-region beg (point))
           (insert (format "#'%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-dot
           (delete-region beg (point))
           (insert "."))
          (clojure-lambda
           (delete-region beg (point))
           (insert (format "#%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-set
           (delete-region beg (point))
           (insert (format "#{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-object
           (delete-region beg (point))
           (insert (format "#object[%s]" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (splice
           (delete-region beg (point))
           (insert
            (nth 2 sxp)
            (lispy--splice-to-str (car (nthcdr 4 sxp)))
            (nth 3 sxp))
           (goto-char beg))
          (clojure-deref-map
           (delete-region beg (point))
           (insert (format "@{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-vector
           (delete-region beg (point))
           (insert (format "@[%s]" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-list
           (delete-region beg (point))
           (insert (format "@(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional-splice
           (delete-region beg (point))
           (insert (format "#?@(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional
           (delete-region beg (point))
           (insert (format "#?(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-comment
           (delete-region beg (point))
           (insert (format "#_%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-comma
           (delete-region beg (point))
           (delete-horizontal-space)
           (insert ", "))
          (racket-true
           (delete-region beg (point))
           (insert "#t"))
          (racket-false
           (delete-region beg (point))
           (insert "#f"))
          (racket-option
           (delete-region beg (point))
           (insert (format "#:%S" (cl-caddr sxp))))
          (angle
           (delete-region beg (point))
           (insert (format "#<%s>" (cl-caddr sxp)))
           (goto-char beg))
          (reference
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (\`
           (if (> (length sxp) 3)
               (progn
                 (goto-char beg)
                 (insert "`")
                 (delete-region (+ (point) 1)
                                (+ (point) 11)))
             (delete-region beg (point))
             (insert "`")
             (prin1 (cl-caddr sxp) (current-buffer)))
           (goto-char beg))
          (\,
           (delete-region beg (point))
           (insert ",")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (comma-splice
           (delete-region beg (point))
           (insert ",@")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (dot
           (delete-region beg (point))
           (insert "."))
          (t (goto-char (1+ beg)))))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\s_\\|\\sw\\)\\(\\\\\\?\\)" nil t)
        (replace-match "?" t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward "\\sw\\(\\\\\\.\\)" nil t)
        (unless (save-match-data
                  (lispy--in-string-p))
          (replace-match "." nil nil nil 1)))
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\(\\\\#\\)" nil t)
        (replace-match "#" nil t nil 1))
      (when lispy-do-fill
        (goto-char (point-min))
        (while (re-search-forward " " nil t)
          (cond ((lispy--in-string-p))

                ((lispy--in-comment-p)
                 (fill-paragraph)
                 (goto-char (cdr (lispy--bounds-comment))))

                ((> (current-column) fill-column)
                 (newline-and-indent)))))
      (goto-char (point-max))
      (widen)))
  (when (and (lispy-right-p)
             (not (lispy--in-comment-p)))
    (backward-list)
    (indent-sexp)
    (forward-list)))


(defun lispy--trim-whitespace-at-bol ()
  "If the point is at '(', remove whitespace (tab and blank space) before point."
  (when (and (looking-at "(")
             (= (point)
                (save-excursion
                  (lispy--out-backward 99)
                  (point))))
    (let ((pt (point)))
      (skip-chars-backward " \t")
      (delete-region pt (point)))))

(defun lispy--get-lisp-indent-function ()
  (if (looking-at "(\\(cl-defun\\|defhydra\\)")
      #'common-lisp-indent-function
    lisp-indent-function))

(defun lispy--prettify-emacs-lisp-sexp ()
  (interactive)
  (let* ((lisp-indent-function (lispy--get-lisp-indent-function))
         (bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (offset (save-excursion (goto-char (car bnd)) (current-column)))
         (was-left (lispy-left-p)))
    (cond ((looking-at (lispy-comment-char 2)))
          (t
           (let* ((max-lisp-eval-depth 10000)
                  (max-specpdl-size 10000)
                  (res (lispy--sexp-normalize (lispy--read str)))
                  (new-str (lispy--prin1-to-string res offset major-mode)))
             (unless (string= str new-str)
               ;; We should not do this if new-str failed to eval.
               (unless (string= "nil" new-str)
                 (delete-region (car bnd)
                                (cdr bnd))
                 (insert new-str))
               (when was-left
                 (backward-list))))))))

(defun lispy--sexp-trim-trailing-newlines (foo comment)
  "Trim trailing (ly-raw newline) from FOO.
Treat comments differently when COMMENT is t."
  (if (and (consp foo) (consp (cdr foo)))
      (let ((expr (reverse foo)))
        (while (and (consp expr)
                    (listp expr)
                    (equal (car expr) '(ly-raw newline))
                    (not (and comment
                              (lispy--raw-comment-p (cadr expr)))))
          (setq expr (cdr expr)))
        (reverse expr))
    foo))

(defun lispy--sexp-normalize (foo)
  "Return a pretty version of FOO.
Only `ly-raw' lists within FOO are manipulated."
  (cond ((null foo)
         nil)

        ((consp foo)
         (cons (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (car foo) t))
               (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (cdr foo) t))))
        (t
         foo)))

(defun lispy--teleport (beg end endp regionp)
  "Move text from between BEG END to point.
Leave point at the beginning or end of text depending on ENDP.
Make text marked if REGIONP is t."
  (let ((str (buffer-substring-no-properties beg end))
        (beg1 (1+ (point)))
        (size (buffer-size))
        (deactivate-mark nil))
    (if (and (>= (point) beg)
             (<= (point) end))
        (progn
          (message "Can't teleport expression inside itself")
          (goto-char beg))
      (goto-char beg)
      (delete-region beg end)
      (when (and (eolp)
                 (lispy-bolp))
        (delete-region (line-beginning-position)
                       (1+ (point))))
      (when (> beg1 beg)
        (cl-decf beg1 (- size (buffer-size))))
      (goto-char beg1)
      (when (looking-at lispy-left)
        (save-excursion
          (newline-and-indent)))
      (unless (lispy-looking-back "[ ([{]")
        (insert " ")
        (cl-incf beg1))
      (insert str)
      (unless (looking-at "[\n)]")
        (insert "\n")
        (backward-char))
      (lispy-save-excursion
        (lispy--reindent 1)
        (goto-char (1- beg1))
        (indent-sexp))
      (if regionp
          (progn
            (setq deactivate-mark nil)
            (set-mark-command nil)
            (goto-char beg1)
            (when endp
              (exchange-point-and-mark)))
        (unless endp
          (goto-char beg1)
          (skip-chars-forward "'"))))))

(defun lispy--swap-regions (bnd1 bnd2)
  "Swap buffer regions BND1 and BND2.
Return a cons of the new text cordinates."
  (when (> (car bnd1) (car bnd2))
    (cl-rotatef bnd1 bnd2))
  (let ((str1 (lispy--string-dwim bnd1))
        (str2 (lispy--string-dwim bnd2)))
    (goto-char (car bnd2))
    (delete-region (car bnd2) (cdr bnd2))
    (insert str1)
    (when (lispy--in-comment-p)
      (unless (eolp)
        (newline-and-indent)))
    (goto-char (car bnd1))
    (delete-region (car bnd1) (cdr bnd1))
    (insert str2)
    (goto-char (car bnd1)))
  (let* ((l1 (- (cdr bnd1) (car bnd1)))
         (l2 (- (cdr bnd2) (car bnd2)))
         (new-beg (+ (car bnd2) (- l2 l1)))
         (new-end (+ new-beg l1)))
    (cons
     (cons (car bnd1) (+ (car bnd1) l2))
     (cons new-beg new-end))))

(defun lispy--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun lispy--delete-pair-in-string (left right)
  "Delete a pair of LEFT and RIGHT in string."
  (let ((bnd (lispy--bounds-string)))
    (when bnd
      (let ((pos (cond ((looking-at left)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-forward right (cdr bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b2 e2)
                              (delete-region b1 e1)
                              b1))))
                       ((looking-at right)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-backward left (car bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b1 e1)
                              (delete-region b2 e2)
                              (+ (point) (- b1 e2)))))))))
        (when pos
          (goto-char pos))))))

(defvar macrostep-keymap)
(defvar lispy--compat-cmd nil
  "Store the looked up compat command.")

(defun lispy--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override))
        (inserter (plist-get plist :inserter)))
    `(lambda ()
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       (interactive)
       ,@(when disable `((,disable -1)))
       (unless (looking-at lispy-outline)
         (lispy--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ,@(when (memq 'god-mode lispy-compat)
                     '(((and (or (bound-and-true-p god-global-mode)
                                 (bound-and-true-p god-local-mode)))
                        (call-interactively 'god-mode-self-insert))))

             ,@(when (memq 'macrostep lispy-compat)
                     '(((and (bound-and-true-p macrostep-mode)
                         (setq lispy--compat-cmd (lookup-key macrostep-keymap (this-command-keys))))
                        (call-interactively lispy--compat-cmd))))

             ,@(when (memq 'magit-blame-mode lispy-compat)
                     '(((and (bound-and-true-p magit-blame-mode)
                         (setq lispy--compat-cmd (lookup-key magit-blame-mode-map (this-command-keys))))
                        (call-interactively lispy--compat-cmd))))

             ((region-active-p)
              (call-interactively ',def))

             ((lispy--in-string-or-comment-p)
              (setq this-command 'self-insert-command)
              (call-interactively 'self-insert-command))

             ((or (lispy-left-p)
                  (lispy-right-p)
                  (and (lispy-bolp)
                       (or (looking-at lispy-outline-header)
                           (looking-at lispy-outline))))
              (call-interactively ',def))

             (t
              (setq this-command 'self-insert-command)
              (call-interactively
               (quote
                ,(or inserter
                     'self-insert-command))))))))

(defun lispy--find-unmatched-delimiters (beg end)
  "Return the positions of unmatched delimiters between BEG and END.
When the region is a greater size than `lispy-safe-threshold', it will not be
checked and nil will be returned."
  (if (> (- end beg) lispy-safe-threshold)
      nil
    (save-excursion
      (goto-char beg)
      (let ((lispy-delimiters (concat (substring lispy-right 0 -1)
                                      "\""
                                      (substring lispy-left 1)))
            matched-left-quote-p
            string-bounds
            string-end
            comment-end
            left-positions
            right-positions)
        (while (re-search-forward lispy-delimiters end t)
          (let* ((match-beginning (match-beginning 0))
                 (matched-delimiter (buffer-substring-no-properties
                                     match-beginning
                                     (match-end 0))))
            (cond
              ((and lispy-safe-actions-ignore-strings
                    (save-excursion
                      (goto-char match-beginning)
                      (setq string-bounds (lispy--bounds-string))
                      (setq string-end (cdr string-bounds))))
               (setq matched-left-quote-p (= (1- (point))
                                             (car string-bounds)))
               (cond ((< (1- string-end) end)
                      (goto-char string-end)
                      ;; when skipping strings, will only match right quote
                      ;; if left quote is not in the region
                      (when (not matched-left-quote-p)
                        (push (1- string-end) right-positions)))
                     (t
                      (when matched-left-quote-p
                        ;; unmatched left quote
                        (push match-beginning left-positions))
                      (goto-char end))))
              ((and lispy-safe-actions-ignore-comments
                    (save-excursion
                      (goto-char match-beginning)
                      (setq comment-end (cdr (lispy--bounds-comment)))))
               (if (< comment-end end)
                   (goto-char comment-end)
                 (goto-char end)))
              (t
               (unless (looking-back "\\\\." (- (point) 2))
                 (if (or (string-match lispy-left matched-delimiter)
                         (and (string= matched-delimiter "\"")
                              (lispy--in-string-p)))
                     (push match-beginning left-positions)
                   (if (> (length left-positions) 0)
                       (pop left-positions)
                     (push match-beginning right-positions))))))))
        (nreverse (append left-positions right-positions))))))

(defun lispy--maybe-split-safe-region (beg end &optional end-unsafe-p)
  "Return a list of regions between BEG and END that are safe to delete.
It is expected that there are no unmatched delimiters in between BEG and END.
Split the region if deleting it would pull unmatched delimiters into a comment.
Specifically, split the region if all of the following are true:

- `lispy-safe-actions-no-pull-delimiters-into-comments' is non-nil
- BEG is inside a comment
- END is not in a comment
- Either there are unmatched delimiters on the line after END or END-UNSAFE-P is
  non-nil

Otherwise, just return a list with the initial region. The regions are returned
in reverse order so that they can be easily deleted without recalculation."
  (if (and lispy-safe-actions-no-pull-delimiters-into-comments
           ;; check that BEG is inside a comment
           ;; `lispy--in-comment-p' returns t at comment start which is
           ;; unwanted here
           (and (save-excursion
                  (nth 4 (syntax-ppss beg))))
           (save-excursion
             (goto-char end)
             ;; check that END is not inside or a comment and that the
             ;; following line has unmatched delimiters or has been specified
             ;; as unsafe to pull into a comment
             (and (not (lispy--in-comment-p))
                  (or end-unsafe-p
                      (lispy--find-unmatched-delimiters
                       end
                       (line-end-position))))))
      ;; exclude newline; don't pull END into a comment
      (let ((comment-end-pos (save-excursion
                               (goto-char beg)
                               (cdr (lispy--bounds-comment)))))
        (list (cons (1+ comment-end-pos) end)
              (cons beg comment-end-pos)))
    (list (cons beg end))))

(defun lispy--find-safe-regions (beg end)
  "Return a list of regions between BEG and END that are safe to delete.
The regions are returned in reverse order so that they can be easily deleted
without recalculation."
  (let ((unmatched-delimiters (lispy--find-unmatched-delimiters beg end))
        (maybe-safe-pos beg)
        safe-regions)
    (dolist (unsafe-pos unmatched-delimiters)
      (unless (= maybe-safe-pos unsafe-pos)
        (setq safe-regions
              (nconc (lispy--maybe-split-safe-region maybe-safe-pos unsafe-pos
                                                     t)
                     safe-regions)))
      (setq maybe-safe-pos (1+ unsafe-pos)))
    (setq safe-regions
          (nconc (lispy--maybe-split-safe-region maybe-safe-pos end)
                 safe-regions))))

(defun lispy--maybe-safe-delete-region (beg end)
  "Delete the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-delete
      (let ((safe-regions (lispy--find-safe-regions beg end)))
        (dolist (safe-region safe-regions)
          (delete-region (car safe-region) (cdr safe-region))))
    (delete-region beg end)))

(defun lispy--maybe-safe-kill-region (beg end)
  "Kill the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-delete
      (let ((safe-regions (lispy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (lispy--string-dwim safe-region) safe-strings)
          (delete-region (car safe-region) (cdr safe-region)))
        (kill-new (apply #'concat safe-strings)))
    (kill-region beg end)))

(defun lispy--maybe-safe-region (beg end)
  "Return the text from BEG to END.
If `lispy-safe-copy' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-copy
      (let ((safe-regions (lispy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (lispy--string-dwim safe-region) safe-strings))
        (apply #'concat safe-strings))
    (lispy--string-dwim (cons beg end))))

(defvar lispy--pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")))

(defun lispy--balance (text)
  "Return TEXT with unmatched delimiters added to the beginning or end.
This does not attempt to deal with unbalanced double quotes as it is not always
possible to infer which side the missing quote should be added to."
  (let ((old-major-mode major-mode))
    (with-temp-buffer
      (funcall old-major-mode)
      (insert text)
      (let ((unmatched-positions
             (lispy--find-unmatched-delimiters (point-min) (point-max)))
            add-to-beginning
            add-to-end
            delim)
        (dolist (pos unmatched-positions)
          (setq delim (buffer-substring pos (1+ pos)))
          (cond ((string-match lispy-left delim)
                 (push (cdr (assoc delim lispy--pairs))
                       add-to-end))
                ((string-match lispy-right delim)
                 (push (car (rassoc delim lispy--pairs))
                       add-to-beginning))))
        (when add-to-beginning
          (goto-char (point-min))
          (insert (apply #'concat add-to-beginning)))
        (when add-to-end
          (goto-char (point-max))
          (when (and lispy-safe-actions-no-pull-delimiters-into-comments
                     (lispy--in-comment-p))
            (push "\n" add-to-end))
          (insert (apply #'concat add-to-end)))
        (buffer-substring (point-min) (point-max))))))

(defun lispy--maybe-safe-current-kill ()
  "Return the most recent kill.
If `lispy-safe-paste' is non-nil, any unmatched delimiters will be added to it."
  (if lispy-safe-paste
      (lispy--balance (current-kill 0))
    (current-kill 0)))

(defun lispy-insert-at-end-of-sexp ()
  (interactive)
  (when (lispy-left-p)
    (progn (lispy-other)
           (backward-char))))


;;; Handlers helpers

(defun lispy--get-handlers ()
  "Return the appropriate handlers for the current buffer.
This is done by iterating over `lispy--handlers-alist` and finding
the first value for which `decider-fn` returns a truthy value, or `nil`
if there is no such value."
  (cl-find-if (lambda (e)
                (let* ((config (cdr e))
                       (decider-fn (assoc-default :decider-fn config)))
                  (funcall decider-fn)))
              lispy--handlers-alist))


;;; Evaluation

(defun lispy-eval ()
  "Evaluate the current sexp after point (if the point is right before a sexp),
before it (if the point is right after a sexp) or the current region (if the region is active).

The evaluation function is defined by `lispy--handlers-alist`."
  (interactive)
  (let ((eval-last-sexp-handler (assoc-default :eval-last-sexp (lispy--get-handlers)))
        (eval-region-handler (assoc-default :eval-region (lispy--get-handlers))))
    (cond
     ((and (region-active-p)
           (not eval-region-handler))
      (lispy--complain-missing-eval-handler))
     ((and (region-active-p)
           eval-region-handler)
      (call-interactively eval-region-handler))
     ((and (not (region-active-p))
           (not eval-last-sexp-handler))
      (lispy--complain-missing-eval-handler))
     ((lispy-left-p)
      (save-excursion
        (lispy-forward 1)
        (call-interactively eval-last-sexp-handler)))
     ((lispy-right-p)
      (call-interactively eval-last-sexp-handler)))))

(defun lispy-eval-buffer ()
  "Evaluate the buffer.

The evaluation function is defined by `lispy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :eval-buffer (lispy--get-handlers))))
      (call-interactively handler)
    (lispy--complain-not-supported)))

(defun lispy-eval-defun ()
  "Evaluate the top level form.

The evaluation function is defined by `lispy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :eval-defun (lispy--get-handlers))))
      (call-interactively handler)
    (lispy--complain-not-supported)))


;;; Describe symbol

(declare-function cider-doc-lookup "ext:cider-doc")
(defun lispy--cider-describe-symbol ()
  (interactive)
  (require 'cider-doc)
  (cider-doc-lookup (lispy--current-function)))

(defun lispy-describe ()
  "Describes the symbol at point.

The function used for describing is defined by `lispy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :describe-symbol (lispy--get-handlers))))
      (call-interactively handler)
    (lispy--complain-not-supported)))

(defun lispy--inf-clojure-describe-symbol ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (call-interactively 'inf-clojure-show-var-documentation)))

(defun lispy--emacs-lisp-describe-symbol ()
  (interactive)
  (let ((symbol (intern-soft (lispy--current-function))))
    (cond ((fboundp symbol)
           (describe-function symbol))
          ((boundp symbol)
           (describe-variable symbol)))))


;;; Recentering

;; mostly taken from evil-scroll-line-to-{top, center, bottom}
(defun lispy-scroll-line-to-top ()
  "Scrolls the current line to the top the window."
  (interactive)
  (let ((line (line-number-at-pos (point)))
        (col (current-column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter (1- (max 1 scroll-margin)))
    (move-to-column col)))

(defun lispy-scroll-line-to-center ()
  "Scrolls the current line to the top the window."
  (interactive)
  (let ((col (current-column)))
    (recenter nil)
    (move-to-column col)))

(defun lispy-scroll-line-to-bottom ()
  "Scrolls the current line to the bottom of the window."
  (interactive)
  (let ((line (line-number-at-pos (point)))
        (col (current-column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter (- (max 1 scroll-margin)))
    (move-to-column col)))

(defun lispy-scroll-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
z: Scroll line to center
t: Scroll line to top
b: Scroll line to bottom
\n")
    (?z (progn (call-interactively 'lispy-scroll-line-to-center)
               (message nil)))
    (?t (progn (call-interactively 'lispy-scroll-line-to-top)
               (message nil)))
    (?b (progn (call-interactively 'lispy-scroll-line-to-bottom)
               (message nil)))
    (t (lispy--complain-unrecognized-key))))


;;; Pretty printing

(defun lispy--prettify-1 ()
  "Normalize/prettify current sexp."
  (lispy--trim-whitespace-at-bol)
  (when-let ((handler (assoc-default :indent-sexp (lispy--get-handlers))))
    ;; if `handler` is not set, simply do nothing - no need to make lispy complain,
    ;; since this function is used internally and we don't want to spam the *Messages* buffer.
    (call-interactively handler)))

(declare-function clojure-align "ext:clojure-mode")
(declare-function clojure-indent-region "ext:clojure-mode")
(defun lispy-clojure-indent ()
  "Indents the next or previous sexp, depending on the point."
  (interactive)
  (if (not (or (lispy-right-p) (lispy-left-p)))
      (lispy-complain "Point is not at beginning or end of sexp.")
    (let* ((beg (if (lispy-right-p)
                    (save-excursion (backward-sexp) (point))
                  (point)))
           (end (if (lispy-right-p)
                    (point)
                  (save-excursion (forward-sexp) (point)))))
      (clojure-indent-region beg end))))

;;* Key definitions
(defvar ac-trigger-commands '(self-insert-command))

(defadvice ac-handle-post-command (around ac-post-command-advice activate)
  "Don't `auto-complete' when region is active."
  (unless (region-active-p)
    ad-do-it))

(defun lispy--delsel-advice (orig-fun)
  "Advice for `delete-selection-mode'.
Usage:
 (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice)"
  (if (and (use-region-p)
           (string-match-p "^special" (symbol-name this-command)))
      (progn
        (delete-active-region)
        (setq this-command 'ignore)
        (self-insert-command 1))
    (funcall orig-fun)))

(defun lispy--undo-tree-advice (&optional _arg)
  "Advice to run before `undo-tree-undo'.

Otherwise, executing undo in middle of a lispy overlay operation
irreversibly corrupts the undo tree state. "
  (lispy-map-delete-overlay))

(advice-add 'undo-tree-undo :before 'lispy--undo-tree-advice)

(defun lispy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (require 'eldoc)
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                (lispy--insert-or-call def plist))))
    (add-to-list 'ac-trigger-commands func)
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(defun lispy-move-and-slurp-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
h: Move left
l: Move right
j: Slurp down
k: Slurp up
\n")
    (?h (progn (call-interactively 'lispy-move-left)
               (message nil)))
    (?j (progn (call-interactively 'lispy-down-slurp)
               (message nil)))
    (?k (progn (call-interactively 'lispy-up-slurp)
               (message nil)))
    (?l (progn (call-interactively 'lispy-move-right)
               (message nil)))
    (t (lispy--complain-unrecognized-key))))

(defun lispy-go-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
g: Go to first defun
n: Narrow
w: Widen
\n")
    (?g (call-interactively 'lispy-go-to-first-defun))
    (?n (progn (call-interactively 'lispy-narrow)
               (message nil)))
    (?w (progn (call-interactively 'lispy-widen)
               (message nil)))
    (t (lispy--complain-unrecognized-key))))

(defvar lispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    ;; getting out of special mode
    (lispy-define-key map "A" 'lispy-insert-at-end-of-sexp)
    (lispy-define-key map "a" 'forward-char)
    (lispy-define-key map "o" 'lispy-open-line-below)
    (lispy-define-key map "O" 'lispy-open-line-above)
    ;; navigation
    (lispy-define-key map "h" 'lispy-step-out)
    (lispy-define-key map "l" 'lispy-step-in)
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)
    (lispy-define-key map "W" 'lispy-knight-up)
    (lispy-define-key map "S" 'lispy-knight-down)
    (lispy-define-key map "I" 'lispy-beginning-of-defun)
    (lispy-define-key map "b" 'lispy-back)
    (lispy-define-key map "L" 'lispy-right)
    (lispy-define-key map "G" 'lispy-go-to-last-defun)
    ;; code actions
    (lispy-define-key map "=" 'lispy-tab)
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "D" 'lispy-eval-defun)
    (lispy-define-key map "B" 'lispy-eval-buffer)
    (lispy-define-key map "K" 'lispy-describe)
    ;; transformations
    (lispy-define-key map "r" 'lispy-raise)
    (lispy-define-key map "R" 'lispy-raise-some)
    (lispy-define-key map "p" 'lispy-move-up)
    (lispy-define-key map "n" 'lispy-move-down)
    (lispy-define-key map "x" 'lispy-splice)
    (lispy-define-key map "+" 'lispy-join)
    (lispy-define-key map "C" 'lispy-convolute)
    (lispy-define-key map "J" 'lispy-oneline)
    (lispy-define-key map "M" 'lispy-alt-multiline)
    ;; barfing & slurping
    (lispy-define-key map ">" 'lispy-slurp)
    (lispy-define-key map "<" 'lispy-barf)
    (lispy-define-key map "s" 'lispy-move-and-slurp-actions)
    ;; acing
    (lispy-define-key map "f" 'lispy-ace-symbol)
    (lispy-define-key map "-" 'lispy-ace-subword)
    (lispy-define-key map "F" 'lispy-ace-symbol-beginning-of-defun)
    (lispy-define-key map "Q" 'lispy-ace-char)
    (lispy-define-key map "q" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit))))
    ;; copying & yanking
    (lispy-define-key map "y" 'lispy-copy)
    (lispy-define-key map "w" 'lispy-clone)
    (lispy-define-key map "P" 'lispy-paste)
    ;; marking
    (lispy-define-key map "m" 'lispy-mark-list)
    ;; misc
    (lispy-define-key map "_" 'lispy-underscore)
    (lispy-define-key map "g" 'lispy-go-actions)
    (define-key map (kbd "SPC") 'lispy-space)
    (lispy-define-key map "u" 'lispy-undo)
    (lispy-define-key map "z" 'lispy-scroll-actions)
    (lispy-define-key map "." 'lispy-repeat)
    ;; magic
    (lispy-define-key map "t" 'lispy-teleport)
    ;; TODO: other
    ;; (lispy-define-key map "S" 'lispy-stringify)
    ;; (lispy-define-key map "D" 'pop-tag-mark)
    ;; (define-key map (kbd "C-8") 'lispy-parens-down)
    ;; (define-key map (kbd "C-9") 'lispy-out-forward-newline)
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

(defvar lispy-mode-map-base
  (let ((map (make-sparse-keymap)))
    ;; killing
    (define-key map (kbd "C-k") 'lispy-kill)
    (define-key map (kbd "M-d") 'lispy-kill-word)
    (define-key map (kbd "M-DEL") 'lispy-backward-kill-word)
    ;; misc
    (define-key map (kbd ";") 'lispy-comment)
    (define-key map (kbd "(") 'lispy-parens)
    (define-key map (kbd "[") 'lispy-brackets)
    (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    map))

(declare-function View-quit "view")

(defun lispy-special ()
  (interactive)
  (cond ((lispy-left-p) (lispy-other))
        ((lispy-right-p) (lispy-other))
        ('t (lispy-backward 1))))

(defvar lispy-mode-map-lispy
  (let ((map (copy-keymap lispy-mode-map-base)))
    ;; navigation
    (define-key map (kbd "<backtab>") 'lispy-special)
    ;; deleting
    (define-key map (kbd "C-d") 'lispy-delete)
    (define-key map (kbd "DEL") 'lispy-delete-backward)
    ;; transformation
    (define-key map (kbd "M-j") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    ;; marking
    (define-key map (kbd "M-m") 'lispy-mark-symbol)
    (define-key map (kbd "C-M-,") 'lispy-mark)
    ;; insert
    (define-key map (kbd "{") 'lispy-braces)
    (define-key map (kbd "\"") 'lispy-quotes)
    (define-key map (kbd "'") 'lispy-tick)
    (define-key map (kbd "`") 'lispy-backtick)
    (define-key map (kbd "#") 'lispy-hash)
    map))

(defcustom lispy-key-theme '(special lispy)
  "List of key themes used to compose `lispy-mode-map'."
  :type
  '(set
    (const special)
    (radio
     (const lispy))))

(defun lispy-set-key-theme (theme)
  "Set `lispy-mode-map' for according to THEME.
THEME is a list of choices: 'special, 'lispy,"
  (setq lispy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special theme) lispy-mode-map-special)
                (when (memq 'lispy theme) lispy-mode-map-lispy)))))
  (setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map))

(lispy-set-key-theme lispy-key-theme)

(provide 'erica-lispy)

;;; erica-lispy.el ends here
