;;; interactive-shell.el --- interactive shell  -*- lexical-binding: t; -*-

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
;;    - sending commands to interactive shells

;;; Code:


;;; Define custom variables for some aspects of shell interaction

;; group under "bind-interactive-shell"
(defgroup bind-interactive-shell nil
  "Variables for defining the rules of engagement when sending commands
to an interactive shell from a buffer."
  :group 'External
  :prefix 'bind-interactive-shell
  :version '0.1.0)

;; Default name of shell buffer (if nil, always prompt in minibuffer)
(defcustom bind-interactive-shell-buffer-name nil
  "Name of (the buffer containing) the desired interactive shell.

If the buffer by the specified name does not exist, it will typically be
created by opening an ansi-terminal in a new window and running the
command defined by bind-interactive-shell-command.

If this variable is not set, the user will be prompted for a targat
buffer name during each attempt to open a shell or send code to it."
  :group 'bind-interactive-shell
  :type 'string)

;; Default command for the shell buffer
(defcustom bind-interactive-shell-command "/bin/bash"
  "Command to open the desired interactive shell in an ansi-terminal."

  :group 'bind-interactive-shell
  :type 'string)


;;; Define functions for opening shells and running shell commands
(defun parse-target-buffer-name ()
  "Parse the name of the TARGET-BUFFER using user setting or prompt.

The name of the target shell can be set via the variable:
  - bind-interactive-shell-buffer-name

If this value is unset, the user is prompted for an input."

  ; interactively prompt for target buffer name if not set as variable
  (interactive)
  (or bind-interactive-shell-buffer-name (read-buffer
					  "Target buffer: "
					  "*interactive-shell*"
					  nil)))

;; function for getting a pointer to a buffer and creating one if needed
(defun get-or-create-buffer (target-buffer &optional move-cursor and-run)
  "Get or create an interactive shell in TARGET-BUFFER.

If the buffer doesn't exist, it is created and put in a new window.
If the buffer exists but is not visible, a window is created for it.
The name and type of shell are determined by custom variables:
  - bind-interactive-shell-buffer-name: name of shell buffer
  - bind-interactive-shell-command: command for opening the shell
For more information, see corresponding variable docs.

After the target buffer is created or identified, optionally move cursor
to its window if MOVE-CURSOR is t.

Optionally run an additional command given by the function AND-RUN
within the same function call.  The command should accept the name of
the target buffer as its (only) input argument.  The command function
should be chosen in conjunction with whether the cursor is kept in the
original buffer or moved to the target buffer.  For example, to run
something akin to 'process-send-region from the source buffer to the
target shell after creating it, set MOVE-CURSOR to nil.  For commands
like 'process-send-string, the position of the cursor is less relevant."

  ; save the current window
  (defvar original-active-window)
  (setq original-active-window (get-buffer-window))

  ; make sure we have short name (no "*") and long name (with "*")
  (defvar target-buffer-short)
  (if (string-match "^\*.*\*$" target-buffer)
      (setq target-buffer-short (string-trim target-buffer "*" "*"))
      (setq target-buffer-short target-buffer))
  (setq target-buffer (concat "*" target-buffer-short "*"))

  ; check if shell is visible and running, and unhide/create one otherwise
  (cond ((try-completion target-buffer (mapcar #'buffer-name (buffer-list)))

	; if the buffer exists but is not visible, we have to split a window
	  (cond ((not (get-buffer-window target-buffer))

	    ; try and carve out space from a non-active window
	    (when (> (length (window-list)) 1) (other-window 1))

	    ; then create new window for shell buffer
	    (evil-split-buffer target-buffer))))

	; if the buffer doesn't exist to begin with, create it
  	(t

	    ; once again, try to carve out space from non-active window
	    (when (> (length (window-list)) 1) (other-window 1))

	    ; then open a scratch window and turn it into a shell buffer
	    (evil-split-buffer "*scratch*")
	    (ansi-term bind-interactive-shell-command target-buffer-short)))

  ; move back to original window if needed
  (if move-cursor (select-window (get-buffer-window target-buffer))
    (select-window original-active-window))

  ; optionally run function
  (when and-run (funcall and-run target-buffer)))

;; function to send selected text to buffer
(defun evil-send-region-to-selected-buffer (target-buffer)
  "Send selected text as a command to TARGET-BUFFER in evil mode."

  ; if we are in normal state, select line under cursor
  (when (evil-normal-state-p) (evil-visual-line))

  ; extract beginning and end of visual selection, and send to shell
  (let* ((target-range (evil-visual-range))
  	 (BEG (nth 0 target-range))
  	 (END (nth 1 target-range)))
    (process-send-region target-buffer BEG END))

  ; exit visual mode
  (evil-normal-state))

;; unbind C-s and use it as a prefix for terminal shell commands
(global-unset-key (kbd "C-s"))

;; (setq bind-interactive-shell-buffer-name "interactive-shell")
;; la

; use "C-s t" to open ansi-term in emacs state in new window
(global-set-key (kbd "C-s t") '(lambda () (interactive)
				 (setq target-buffer
				       (parse-target-buffer-name))
				 (get-or-create-buffer target-buffer t)
				 (evil-emacs-state)))

;; send region to shell buffer with "C-s x"
(global-set-key (kbd "C-s x")
		'(lambda () (interactive)
		  (setq target-buffer
		    (parse-target-buffer-name))
		  (get-or-create-buffer target-buffer nil
		    'evil-send-region-to-selected-buffer)))

(provide 'interactive-shell)
;;; interactive-shell.el ends here
