;;; bind-terminal-shell.el --- bind terminal shell  -*- lexical-binding: t; -*-

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
(defgroup bind-terminal-shell nil
  "Variables for defining the rules of engagement when sending commands
to a terminal shell from a buffer."
  :group 'External
  :prefix 'bind-terminal-shell
  :version '0.1.0)

;; Name of shell buffer (if nil, always prompt in minibuffer)
(defcustom bind-terminal-shell-buffer-name nil
  "Name of (the buffer containing) the desired terminal shell.

If the buffer by the specified name does not exist, it will typically be
created by opening an ansi-terminal in a new window and running the
command defined by bind-terminal-shell-program.

If this variable is not set, the user will be prompted for a targat
buffer name during each attempt to open a shell or send code to it."
  :group 'bind-terminal-shell
  :type 'string
  :safe (lambda (_) t))

;; Executable program for the shell buffer
(defcustom bind-terminal-shell-program "/bin/bash"
  "Program to run in the terminal shell."

  :group 'bind-terminal-shell
  :type 'string
  :safe (lambda (_) t))

;; initialization commands to run when a terminal first starts
(defcustom bind-terminal-shell-init-commands nil
  "Commands to initialize the terminal."

  :group 'bind-terminal-shell
  :type '(repeat string)
  :safe (lambda (_) t))


;; ;; truncate lines instead of wrapping in terminal mode
;; (add-hook 'term-mode-hook (lambda () (setq truncate-lines t)))

;; automatically switch to char mode in emacs state, line mode otherwise
(add-hook 'evil-emacs-state-entry-hook
	  (lambda () (when (and (derived-mode-p 'term-mode)
				(get-buffer-process (window-buffer)))
		       (term-char-mode))))
(add-hook 'evil-emacs-state-exit-hook
	  (lambda () (when (and (derived-mode-p 'term-mode)
				(get-buffer-process (window-buffer)))
		       (term-line-mode))))

;;; Define functions for opening shells and running shell commands
(defun prompt-target-buffer-name ()
  "Prompt the user for a buffer name, and then return it."

  ;; interactively prompt for target buffer name if not set as variable
  (interactive)
  (read-buffer "Name of shell buffer: " "*interactive-shell*" nil))

;; function for getting a pointer to a buffer and creating one if needed
(defun get-or-create-shell-buffer
    (&optional target-buffer program init-commands move-cursor and-run)
  "Get or create a terminal shell in TARGET-BUFFER.

TARGET-BUFFER can be set via `bind-terminal-shell-buffer-name'.
Otherwise, the user will be prompted for a buffer name.

If the buffer doesn't exist, it is created and put in a new window.
If the buffer exists but is not visible, a window is created for it.
The name of the shell can be set via a custom variable:

  - see: bind-terminal-shell-buffer-name

If unset, the user is prompted for a name in the minibuffer.  This also
makes this command suitable for keybindings if wrapped in an interactive
lambda function.

If a new shell is being created, its working directory will be the same
as the working directory of the buffer from which it is initialized (or
the defaut directory if the source buffer is not a file on disk).

PROGRAM: The program that the shell runs.  It defaults to the value of
the custom variable:

  - bind-terminal-shell-program

Flags are allowed, and the command need not be interactive.  For
example, \"ls -laHF\" will work, but the shell will exit immediately.

INIT-COMMANDS: Optionally, run a set of initialization commands given by
the list INIT-COMMANDS. These commands are executed after the shell
spins up but before any other actions are performed in it.  For example,
in a simple terminal shell, INIT-COMMANDS may be used to source
additional config files.  This variable can be set via:

  - bind-terminal-shell-init-commands

After the target buffer is created or identified, the cursor will be
moved to its window if MOVE-CURSOR is t.

AND-RUN: Optionally run an additional command given by the function
AND-RUN within the same function call.  The command should accept the
name of the target buffer as its (only) input argument.  The command
function should be chosen in conjunction with whether the cursor is kept
in the original buffer or moved to the target buffer.  For example, to
run something akin to 'process-send-region from the source buffer to the
target shell after creating it, set MOVE-CURSOR to nil.  For commands
like 'process-send-string, the position of the cursor is less relevant."

  ;; save the current window
  (defvar original-active-window)
  (setq original-active-window (get-buffer-window))

  ;; figure out the name of the target buffer to use
  (setq
   target-buffer
   (or target-buffer (or bind-terminal-shell-buffer-name
			 (prompt-target-buffer-name))))

  ;; make sure we have short name (no "*") and long name (with "*")
  (defvar target-buffer-short)
  (if (string-match "^\*.*\*$" target-buffer)
      (setq target-buffer-short (string-trim target-buffer "*" "*"))
      (setq target-buffer-short target-buffer))
  (setq target-buffer (concat "*" target-buffer-short "*"))

  ;; check if shell is visible and running, and unhide/create one otherwise
  (cond ((try-completion target-buffer (mapcar #'buffer-name (buffer-list)))

	; if the buffer exists but is not visible, we have to split a window
	  (cond ((not (get-buffer-window target-buffer))

	    ;; try and carve out space from a non-active window
	    (when (> (length (window-list)) 1) (other-window 1))

	    ;; skip treemacs buffer
	    (when (boundp 'treemacs--in-this-buffer)
	      (when treemacs--in-this-buffer (other-window 1)))

	    ;; then create new window for shell buffer
	    (evil-split-buffer target-buffer))))

	; if the buffer doesn't exist to begin with, create it
  	(t

	    ;; save buffer-local variables needed for terminal shell
	    (let* ((current-buffer-tmp (current-buffer))
		   (init-commands-tmp
		    (or init-commands bind-terminal-shell-init-commands))
		   (all-args (split-string-and-unquote
			      (or program bind-terminal-shell-program)))
		   (cmd (pop all-args))
		   (termbuf (apply 'make-term
				   target-buffer-short cmd nil all-args)))

	      ;; once again, try to carve out space from non-active window
	      (when (> (length (window-list)) 1) (other-window 1))
	      (when (boundp 'treemacs--in-this-buffer)
		(when treemacs--in-this-buffer (other-window 1)))
	      ;; then open a scratch window and turn it into a shell buffer
	      (evil-split-buffer current-buffer-tmp)
	      (set-buffer termbuf)
	      (term-mode)
	      (term-char-mode)
	      (switch-to-buffer termbuf)
	      (dolist (each-cmd init-commands-tmp)
		(comint-send-string target-buffer
				    (concat each-cmd "\n")))
	      )))

  ;; move back to original window if needed
  (if move-cursor (select-window (get-buffer-window target-buffer))
    (select-window original-active-window))

  ;; optionally run function
  (when and-run (funcall and-run target-buffer)))

;; (let ((init-commands (or nil (list "source /mnt/projects/statosphere/config"))))
;;   (dolist (each-command init-commands) (message each-command)))

;; function to send selected text to buffer
(defun evil-send-region-to-terminal-shell (target-buffer
					   &optional fallback-selection)
  "Send selected text as a command to TARGET-BUFFER in evil mode.

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
  	 (END (nth 1 target-range)))
    (comint-send-region target-buffer BEG END))

  ;; exit visual mode
  (evil-normal-state))

(provide 'bind-terminal-shell)
;;; bind-terminal-shell.el ends here
