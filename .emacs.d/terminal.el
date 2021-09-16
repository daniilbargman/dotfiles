;;; terminal.el --- embedded terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <daniilbargman@daniilbargman-xps>
;; Keywords: convenience

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

;; Utility scripts and functions for interacting with a terminal shell
;;    - opening terminal shells with custom executables
;;    - sending commands to terminal shells

;;; Code:


;;; Define custom variables for some aspects of shell interaction

;; group under "bind-terminal-shell"
(defgroup terminal nil
  "Settings for the terminal shell."
  :group 'External
  :prefix 'terminal
  :version '0.1.0)

;; Name of shell buffer (if nil, always prompt in minibuffer)
(defcustom terminal-buffer-name nil
  "Name of (the buffer containing) the desired terminal shell.

If the buffer by the specified name does not exist, it will typically be
created by opening an ansi-terminal in a new window and running the
command defined by bind-terminal-shell-program.

If this variable is not set, the user will be prompted for a targat
buffer name during each attempt to open a shell or send code to it."
  :group 'terminal
  :type 'string
  :safe (lambda (_) t))

;; Executable program for the shell buffer
(defcustom terminal-program "/bin/bash"
  "Program to run in the terminal shell."

  :group 'terminal
  :type 'string
  :safe (lambda (_) t))

;; initialization commands to run when a terminal first starts
(defcustom terminal-init-commands nil
  "Commands to initialize the terminal."

  :group 'terminal
  :type '(repeat string)
  :safe (lambda (_) t))


;;; use vterm as the basis
(use-package vterm

  :custom
  (vterm-always-compile-module t)  ; it needs to compile sometimes
  (vterm-shell "/bin/bash") ; this defaults to /bin/sh for some reason
  (vterm-kill-buffer-on-exit t) ; don't keep terminated buffers around
  (vterm-max-scrollback-value 100000) ; maximum scrollback allowed

  :config

  ;; make sure counsel-yank-pop works (inhibit read-only error)
  (with-eval-after-load "ide-base"
    (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
      (if (equal major-mode 'vterm-mode)
	  (let ((inhibit-read-only t)
		(yank-undo-function (lambda (_start _end) (vterm-undo))))
	    (cl-letf (((symbol-function 'insert-for-yank)
		  (lambda (str) (vterm-send-string str t))))
		(apply orig-fun args)))
	(apply orig-fun args)))
    (advice-add 'counsel-yank-pop-action
		:around #'vterm-counsel-yank-pop-action)
    )

  ;; set fill column to 79 in vterm-mode buffers
  :hook (vterm-mode . (lambda () (setq fill-column 79)))

  )

;; use vterm-toggle instead of coding toggling logic manually
(use-package vterm-toggle

  :custom

  ;; do not go to full-screen mode for new terminals
  (vterm-toggle-fullscreen-p nil)

  ;; create new buffer for each new directory
  (vterm-toggle-cd-auto-create-buffer t)

  ;; don't attempt to reconfigure layout after killing vterm buffer
  (vterm-toggle-reset-window-configuration-after-exit nil)

  ;; don't auto-hide terminal if one exists and is visible
  (vterm-toggle-hide-method nil)

  ;; use dedicated buffers
  (vterm-toggle--vterm-dedicated-buffer t)

  :config

  ;; add new vterm buffers at the bottom by default
  ;;
  ;; based on https://github.com/jixiuf/vterm-toggle
  (add-to-list 'display-buffer-alist

      ;; applies to buffers with vterm-mode major mode
    '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))

      ;; changed from `display-buffer-reuse-window' because reuse-mode
      ;; ensures all vterm buffers are created as new tabs in the same
      ;; window and not in new windows, even when dedicated flag is set
      (display-buffer-reuse-mode-window display-buffer-in-direction)
      ;;display-buffer-in-direction/direction/dedicated is added in emacs27
      (direction . bottom)
      ;; (dedicated . t) ;dedicated is supported in emacs27
      (reusable-frames . visible)
      (window-height . 0.35)
      (window-width . 1)
      ))

  )


;;; helper function for determing target vterm buffer name
(defun parse-vterm-buffer-name ()
  "Parse target vterm buffer name for the current buffer."
  (or terminal-buffer-name (concat "vterm-" (buffer-name))))

;;; function for renaming a terminal and running custom init commands
(defun get-or-create-terminal
    (&optional program init-commands move-cursor and-run)
  "Get or create a vterm shell and (optionally) run some commands in it.

The vterm buffer name can be set via `terminal-buffer-name'. Otherwise,
the name will be \"vterm-<name-of-calling-buffer>\".

Buffers are created and managed using 'vterm-toggle'.

PROGRAM: The program that the shell runs. It defaults to the value of
the custom variable `terminal-program' (default: /bin/bash).

Flags are allowed, and the command represented by PROGRAM need not, in
fact, be interactive. For example, 'ls -laHF' will work, but the shell
will exit immediately.

INIT-COMMANDS: Optionally, run a set of initialization commands given by
the list INIT-COMMANDS. These commands are executed after the shell
spins up but before any other actions are performed in it. For example,
in a simple terminal shell, INIT-COMMANDS may be used to source
config files. Init-commands will only be executed in newly created
terminal. Default commands can be set via `terminal-init-commands'.

After the target buffer is created or identified, the cursor will be
moved to its window if MOVE-CURSOR is t.

AND-RUN: Gives the possibility to run an additional elisp command within
the same function call. The command should accept one non-optional input
argument: the name of the target buffer (it will be supplied
automatically by this function). The command function should be chosen
in conjunction with whether the cursor is kept in the original buffer or
moved to the target buffer. For example, to run something akin to
`process-send-region' from the source buffer to the terminal after
creating it, set MOVE-CURSOR to nil."

  (interactive)

  ;; figure out the name of the target buffer to use
  (let*
      (
       (calling-buffer (buffer-name))
       (vterm-buffer-name (parse-vterm-buffer-name))
       (vterm-buffer-existed (get-buffer vterm-buffer-name))
       (vterm-shell
	(or program terminal-program))
       (init-commands-tmp
	(or init-commands terminal-init-commands))
       (num-windows (length (window-list)))
       )

      ;; run init commands only if a buffer doesn't exist
    (unless vterm-buffer-existed

      ;; create buffer and run initialization commands
      (with-current-buffer (vterm-toggle-show)

	;; make this buffer a dedicated one so that each new buffer name
	;; results in a new buffer object. Note: this is different from
	;; the guidelines in the official docs, but it works.
	(setq vterm-toggle--dedicated-p t)

	;; send init commands
	(dolist (each-cmd init-commands-tmp)
	  (comint-send-string vterm-buffer-name
			      (concat each-cmd "\n")))

	)

      ;; hide the buffer; it will be displayed in the right place by
      ;; the display command that comes after this block.
      (let ((bufw (get-buffer-window vterm-buffer-name)))
	(if (> num-windows 1)
	    (switch-to-prev-buffer bufw)
	  (delete-window bufw)
	)
      )

      ;; if there is a command that should be run afterwards, give a
      ;; few seconds for the terminal shell to initialize
      (when and-run (sleep-for 3))

      )

    ;; display buffer
    (display-buffer vterm-buffer-name)

    ;; move back to original window if needed
    (if move-cursor
	(select-window (get-buffer-window vterm-buffer-name))
      (select-window (get-buffer-window calling-buffer)))

    ;; execute remaining elisp commands as requested
    (when and-run (funcall and-run vterm-buffer-name))

    ))


;; function to send selected text to buffer
(defun evil-send-region-to-terminal (term-buffer &optional fallback-selection)
  "Send selected text as a command to TERM-BUFFER in evil mode.

FALLBACK-SELECTION is a function that determines which region should be
sent if there is no visual selection available.  It defaults to
`evil-visual-line', i.e. the default behaviour is to send the line under
cursor if no text is visually selected.  Alternatives could involve
selecting regions of text based on properties, for example
`python-mark-fold-or-section' could be used as to achieve block-wise
execution of python code if the python-x package is installed."

  ;; if we are in normal state, select line under cursor
  (when (evil-normal-state-p)
    (if fallback-selection (funcall fallback-selection)
      (evil-visual-line)))

  ;; extract beginning and end of visual selection, and send to shell
  (let* ((target-range (evil-visual-range))
  	 (BEG (nth 0 target-range))
  	 (END (nth 1 target-range))
	 )
    (comint-send-region term-buffer BEG END))

  ;; exit visual mode
  (evil-normal-state))

(provide 'terminal)
;;; terminal.el ends here
