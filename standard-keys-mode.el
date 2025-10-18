;;; standard-keys-mode.el --- Emulate standard keybindings from modern editors  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Elias Gabriel Pérez

;; Author: Elias G. Pérez <eg642616@gmail.com>
;; Created: 2025-08-21
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/DevelopmentCool2449/standard-keys-mode
;; Keywords: emulations, convenience
;; Version: 1.0.0

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages is yet another cua-like mode, which emulates the
;; standard and common keybindings from modern editors (cut: C-x,
;; copy: C-c, redo: C-y, undo: C-z ...), similar to other packages
;; such as: cua-mode (built-in), ergoemacs-mode and wakib-keys.
;;
;; It rebinds the classic C-c and C-x key prefixes to other
;; keys (C-e and C-d respectively); this can be set to other key
;; prefix you prefer.
;;
;; This package includes some keymap template/themes out-the-box to
;; emulate different editors or modes without having to spend time
;; modifying them.
;;
;; Also this offers some additional functions:
;;
;; - `standard-keys-newline-and-indent-before-point'
;; - `standard-keys-move-beginning-of-line-or-indentation'
;; - `standard-keys-create-new-buffer'
;; - `standard-keys-kill-region-or-line'
;; - `standard-keys-copy-region-or-line'
;;
;; Thanks to Abdulla Bubshait (darkstego) for creating wakib-key,
;; which was the inspiration for this package.

;;; Code:


;;;; User Options

(defgroup standard-keys nil
  "Standard keybindings from modern editors for Emacs."
  :tag "Standard keys"
  :group 'emulations
  :group 'convenience)

(defcustom standard-keys-update-commands-descriptions t
  "If non-nil, commands docstring descriptions should use the remaped \\`C-x'/\\`C-c'."
  :type 'boolean)

(defcustom standard-keys-override-new-C-x-and-C-c-commands nil
  "Whether key definitions in \\`C-x' and \\`C-c' must take precedence over other keymaps.
WARNING: Enabling this may override some terminal specific keybindings
and only should be used only for override properly the `C-x' and `C-c'
in buffers or modes which take precedence over `standard-keys-mode'
keymaps."
  :type 'boolean
  :risky t)

(defcustom standard-keys-new-buffer-mode #'fundamental-mode
  "Which major mode should `standard-keys-create-new-buffer' use.
The value can be any major mode function symbol.

If set to `scratch-buffer', it will create a new *scratch* buffer,
*scratch* buffer major mode is specified in `initial-major-mode'."
  :type '(choice
          function (const :tag "Create a new *scratch* buffer" scratch-buffer)))

(defcustom standard-keys-map-style 'standard-keys-default-keymap
  "Keymap style to use in `standard-keys-mode'.
The value must be a symbol.

It can be any of the following symbols:
 - `standard-keys-default-keymap' (Default).
 - `standard-keys-ergoemacs-like-keymap' Use Ergoemacs-like keybindings.
 - `standard-keys-minimal-keymap' Minimal and simple keymap.

or a custom one:
  (setopt standard-keys-map-style \\='my-custom-map)"
  :type '(radio
          (variable-item standard-keys-default-keymap)
          (variable-item standard-keys-ergoemacs-like-keymap)
          (variable-item standard-keys-minimal-keymap)
          (symbol :tag "Custom keymap")))


;;;; Internal Functions

(defun standard-keys-key-keybinding (key)
  "Execute Dynamic KEY prefix action.
KEY must be a key prefix string, either \"C-x\" or \"C-c\"."
  ;; menu-item is used here for compute the KEY keymap.
  `(,(concat key "-prefix")
    . (menu-item
       ""
       ;; this should avoid to execute the lambda all the time
       ,(keymap-lookup key-translation-map key)
       :filter
       ,(lambda (keymap)
          (make-composed-keymap
           (sk--get-key-in-active-mode-keymaps key)
           keymap)))))

(defun sk--get-key-in-active-mode-keymaps (key &optional maps)
  "Return a list of keymaps from MAPS where prefix KEY is defined.
If MAPS is not set, it will use `current-active-maps' instead."
  (let (list)
    (dolist (keymaps (or maps (current-active-maps)))
      (when-let* (;; Exclude the currently used keymap (avoids consing a new list)
                  ((not (eq (symbol-value standard-keys-map-style) keymaps)))
                  (k (keymap-lookup keymaps key)))
        (push k list)))
    ;; The keymaps order must be inverted
    (nreverse list)))

(defvar standard-keys-C-x-dynamic-prefix (standard-keys-key-keybinding "C-x")
  "Dynamic `C-x' prefix used for the keymaps.")

(defvar standard-keys-C-c-dynamic-prefix (standard-keys-key-keybinding "C-c")
  "Dynamic `C-c' prefix used for the keymaps.")

(defun sk--where-is-prefix-key (prefix map actives)
  "Return a keymap where PREFIX keymap is defined in keymap MAP.
ACTIVES is for internal use only."
  (catch 's-k-key
    (map-keymap-internal
     (lambda (key def)
       (cond
        ((equal def prefix)
         (throw 's-k-key
                `(,key
                  keymap
                  ,@(if (equal prefix sk-C-x-dynamic-prefix)
                        (sk--get-key-in-active-mode-keymaps "C-x" actives)
                      (sk--get-key-in-active-mode-keymaps "C-c" actives)))))

        ((keymapp def)
         (when-let* ((map (sk--where-is-prefix-key prefix def actives)))
           (throw 's-k-key
                  `(,key keymap ,map))))))
     map)))

(defun sk--where-is-internal-advice (orig-fun def &optional keymap &rest rest)
  "Advice for `where-is-internal' to find DEF in the rebinded \\`C-x' and \\`C-c' maps.
ORIG-FUN, KEYMAP and REST are arguments for `where-is-internal'."
  (unless keymap
    (let* ((actives (current-active-maps))
           (map (symbol-value standard-keys-map-style))
           (C-x (sk--where-is-prefix-key sk-C-x-dynamic-prefix map actives))
           (C-c (sk--where-is-prefix-key sk-C-c-dynamic-prefix map actives)))
      (setq keymap
            `(,@actives
              ,(make-composed-keymap
                `(,C-x ,C-c))))))
  (apply orig-fun def keymap rest))


;;;; Commands

;;;###autoload
(defun sk-keyboard-quit ()
  "Quit from the current command/action.
This acts like `C-g' but is intended to be used for any other additional
keybindings.

NOTE: This doesn't work if `C-g' is remaped."
  (interactive)
  (call-interactively (key-binding "\C-g")))

;;;###autoload
(defun sk-newline-and-indent-before-point ()
  "Like `newline-and-indent', but inserts the newline before cursor."
  (interactive "^")
  (beginning-of-line)
  (save-excursion (newline-and-indent))
  (indent-according-to-mode))

;;;###autoload
(defun sk-move-beginning-of-line-or-indentation (arg)
  "Move point to visible beginning of current logical line or indentation.
If point is already at the beginning of the indentation, point is moved
to the beginning of the line, otherwise it is moved to beginning of the
indentation.

ARG is used like in `move-beginning-of-line'."
  (interactive "^p")
  (let ((arg (or arg 1))
        (current-point (point))
        (point (progn (back-to-indentation) (point))))
    (when (= current-point point)
      (move-beginning-of-line arg))))

;;;###autoload
(defun sk-create-new-buffer ()
  "Create a new Untitled empty buffer.
The buffer major mode is specified in `standard-keys-new-buffer-mode'."
  (interactive)
  (let ((buf (generate-new-buffer "Untitled")))
    (with-current-buffer buf
      (if (eq standard-keys-new-buffer-mode 'scratch-buffer)
          (progn
            (funcall initial-major-mode)
            (when initial-scratch-message
              (insert (substitute-command-keys initial-scratch-message))
              (set-buffer-modified-p nil))
            (when (eq initial-major-mode 'lisp-interaction-mode)
              (setq-local trusted-content :all)))
        (funcall standard-keys-new-buffer-mode)))
    (switch-to-buffer buf)))

;;;###autoload
(defun sk-copy-region-or-line ()
  "Copy the active region, or the current line if no region is active."
  (interactive)
  (let* ((region (use-region-p))
         (beg (line-beginning-position))
         (end (line-beginning-position 2)))
    (unless region (pulse-momentary-highlight-region beg end 'region))
    (kill-ring-save beg end region)))

;;;###autoload
(defun sk-cut-region-or-line ()
  "Cut the active region, or the current line if no region is active."
  (interactive)
  (kill-region (line-beginning-position)
               (line-beginning-position 2)
               (use-region-p)))


;;;; Keymaps

;; Probably this should be moved
;; to another file if gets too long.

;;; Mode keymaps
(defvar-keymap standard-keys-default-keymap
  :doc "Default keymap used in `standard-keys-map-style'."
  "C-o"   #'find-file
  "C-S-o" #'revert-buffer
  "C-w"   #'kill-current-buffer
  "C-q"   #'save-buffers-kill-terminal
  ;; C-c and C-x remapping
  "C-e" standard-keys-C-x-dynamic-prefix
  "C-d" standard-keys-C-c-dynamic-prefix

  "C-x"   #'standard-keys-cut-region-or-line
  "C-c"   #'standard-keys-copy-region-or-line
  "C-v"   #'yank
  "C-z"   #'undo-only
  "C-y"   #'undo-redo
  "C-S-z" #'undo-redo
  "C-f"   #'isearch-forward
  "C-S-f" #'isearch-backward
  "C-r"   #'query-replace
  "C-S-r" #'query-replace-regexp
  "C-s"   #'save-buffer
  "C-S-s" #'write-file
  "C-p"   #'print-buffer
  "C-a"   #'mark-whole-buffer
  "C-+"   #'text-scale-increase
  "C--"   #'text-scale-decrease
  "C-="   #'text-scale-adjust
  "C-n"   #'standard-keys-create-new-buffer
  "C-;"   #'comment-line
  "M-0"   #'delete-window
  "M-1"   #'delete-other-windows
  "M-2"   #'split-window-below
  "M-3"   #'split-window-right
  "C-<return>"   #'rectangle-mark-mode
  "C-S-<return>" #'standard-keys-newline-and-indent-before-point
  "C-b"      #'switch-to-buffer
  "<home>"   #'standard-keys-move-beginning-of-line-or-indentation
  "<escape>" #'standard-keys-keyboard-quit)

(defvar-keymap standard-keys-minimal-keymap
  :doc "Minimal and basic CUA-like keymap for `standard-keys-map-style'.
This keymap is intended to be a minimal CUA, binding only a few
keybindings, and remaping `C-x' and `C-c' to `Control Shift x' and
`Control Shift c'."
  "C-S-x" standard-keys-C-x-dynamic-prefix
  "C-S-c" standard-keys-C-c-dynamic-prefix

  "C-s" #'save-buffer
  "C-x" #'kill-region
  "C-c" #'kill-ring-save
  "C-v" #'yank
  "C-z" #'undo-only
  "C-y" #'undo-redo)

(defvar-keymap standard-keys-ergoemacs-like-keymap
  :doc "`Ergoemacs QWERTY US layout'-like keymap for `standard-keys-map-style'.
*This is not a complete emulation*, it just provides some basic
keybindings from ergoemacs."
  ;; Meta (+ Shift) keys
  "M-4" #'split-window-right
  "M-e" #'backward-kill-word
  "M-r" #'kill-word
  "M-t" #'completion-at-point
  "M-y" #'isearch-forward
  "M-u" #'backward-word
  "M-i" #'previous-line ; Movement keys
  "M-k" #'next-line
  "M-j" #'left-char
  "M-l" #'right-char
  "M-o" #'forward-word
  "M-p" #'recenter
  "M-a" #'execute-extended-command
  "M-d" #'delete-backward-char
  "M-f" #'delete-forward-char
  "M-x" #'standard-keys-cut-region-or-line
  "M-n" #'beginning-of-buffer
  "M-SPC" #'set-mark-command
  ;; Meta + Shift keys
  "M-S-4" #'split-window-below
  "M-S-y" #'isearch-backward
  "M-S-u" #'move-beginning-of-line
  "M-S-o" #'move-end-of-line
  "M-S-i" #'forward-page
  "M-S-j" #'backward-page
  "M-S-n" #'end-of-buffer
  ;; Fn keys
  "<f1>" #'execute-extended-command
  "<f6>" esc-map ;; (?)
  ;; Misc
  "ESC" #'standard-keys-keyboard-quit ; Probably not from ergoemacs
  ;; <menu> prefixes and commands
  "<menu> d" standard-keys-C-x-dynamic-prefix
  "<menu> f" standard-keys-C-c-dynamic-prefix
  "<menu> g" #'universal-argument
  "<menu> q" #'quoted-insert ; Not from ergoemacs
  ;; Control keys
  "C-w" #'kill-current-buffer
  "C-r" #'revert-buffer-quick
  "C-y" #'undo-redo
  "C-o" #'find-file
  "C-p" #'print-buffer
  "C-a" #'mark-whole-buffer
  "C-s" #'save-buffer
  "C-f" #'isearch-forward ; isearch-forward is more powerful than
                          ; search-forward in ergoemacs
  "C-l" #'goto-line
  "C-z" #'undo
  "C-x" #'standard-keys-cut-region-or-line
  "C-c" #'standard-keys-copy-region-or-line
  "C-v" #'yank
  "C-n" #'standard-keys-create-new-buffer
  "C-." #'save-buffers-kill-terminal
  "C-/" #'info
  ;; Control + Shift keys
  "C-S-w" #'delete-frame
  "C-S-s" #'write-file
  "C-S-f" #'occur
  "C-S-n" #'make-frame-command)


;;;; Minor mode definition

(defvar sk--emulation-keymap nil
  "This variable is intended to be placed in `emulation-mode-map-alists'.")

(defvar sk--bk-otl-value nil
  "Backup value from `overriding-terminal-local-map'.")

(defvar sk--overriding-map)

(defun sk--override-new-C-x-C-c-bindings ()
  "Override the new `C-x' and `C-c' bindings.
Add new `C-c' and `C-x' bindings to `overriding-terminal-local-map' if
`standard-keys-override-new-C-x-and-C-c-commands' is non-nil."
  (when sk-override-new-C-x-and-C-c-commands
    (let* ((C-c (keymap-lookup (symbol-value standard-keys-map-style) "C-c"))
           (C-x (keymap-lookup (symbol-value standard-keys-map-style) "C-x"))
           (list (append
                  (if C-c (list "C-c" C-c))
                  (if C-x (list "C-x" C-x)))))
      (when list
        (setq
         sk--bk-otl-value overriding-terminal-local-map
         sk--overriding-map (apply #'define-keymap list)
         overriding-terminal-local-map sk--overriding-map)))))

;;;###autoload
(define-minor-mode standard-keys-mode
  "Emulate standard keybindings from modern editors."
  :global t
  :lighter " SK"
  (cond
   (noninteractive
    (setq standard-keys-mode nil))
   (sk-mode
    (setq sk--emulation-keymap `((sk-mode . ,(symbol-value standard-keys-map-style))))
    (add-to-ordered-list 'emulation-mode-map-alists 'sk--emulation-keymap 400)

    (standard-keys--override-new-C-x-C-c-bindings)

    (when standard-keys-update-commands-descriptions
      (advice-add #'where-is-internal :around #'sk--where-is-internal-advice)))

   (t
    (setq emulation-mode-map-alists (delq 'sk--emulation-keymap emulation-mode-map-alists))

    (when sk-override-new-C-x-and-C-c-commands
      (setq overriding-terminal-local-map sk--bk-otl-value))

    (when sk-update-commands-descriptions
      (advice-remove #'where-is-internal #'sk--where-is-internal-advice)))))

(provide 'standard-keys-mode)
;;; standard-keys-mode.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sk-" . "standard-keys-"))
;; End:
