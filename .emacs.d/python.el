;;; python.el --- python settings -*- lexical-binding: t; -*-


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

;; settings for python development

;;; Code:


;;; Define custom variables for python

;; group under "my-python"
(defgroup my-python nil
  "Variables for personal customizations of python behaviour."
  :group 'External
  :prefix 'my-python
  :version '0.1.0)

;; Name of shell buffer
(defcustom my-python-shell-buffer-name nil
  "Name of (the buffer containing) the desired python shell.

Locally overrides terminal-buffer-name defined in terminal.el.

This variable is buffer-local."
  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))
(make-variable-buffer-local 'my-python-shell-buffer-name)

;; Name of shell buffer (if nil, always prompt in minibuffer)
(defcustom my-python-shell-buffer-name-default-prefix "ipython"
  "Prefix for the default python shell buffer name.

Locally overrides terminal-buffer-name defined in terminal.el."
  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))

;; Executable program for the shell buffer
(defcustom my-python-shell-program "python"
  "Program that runs the python shell.

Accepts any valid bash terminal command, including flags.

Also accepts a wildcard argument \"k8s\" which instructs the function to
parse an executable command using `k8s-parse-exec-command' from k8s.el.
Requires `my-python-k8s-pod-label' and `my-python-k8s-pod-namespace' to
be set.  Optionally accepts `my-python-k8s-exec-command' as well."

  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-pod-label nil
  "Label to identify a K8S pod that should run a python shell.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-pod-namespace nil
  "K8S namespace containing the pod that should run a python shell.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-exec-command nil
  "Shell command for the K8S pod that should launch the python shell.

If unset, it defaults to `/bin/sh' per the `k8s-parse-exec-command'
function spec.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :local t
  :safe (lambda (_) t))

;; initialization commands to run when a python shell first starts
(defcustom my-python-shell-init-commands nil
  "Commands to initialize the python shell."

  :group 'my-python
  :type '(repeat string)
  :local t
  :safe (lambda (_) t))

;; set sane buffer-local configurations
(add-hook
 'python-mode-hook
 (lambda ()
   (setq fill-column 79
	 evil-shift-width 4
	 ide-format-parens-opening-paren-alist '("(" "[" "{")
	 ide-paren-wrap-delimiters
	 '(((before " for ") (before " if ") (before " else "))
	   ((before " and ") (before " or "))
	   ((after ",")))
	 )
   )
 )

;; use the pyright language server
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))  ; or lsp
  :custom
  ;; (lsp-pyright-langserver-command-args '("--stdio" "--verbose"))

  ;; autoimport completions clutter company mode intolerably
  (lsp-pyright-auto-import-completions nil)

  ;; type-checking is not necessary for now
  (lsp-pyright-typechecking-mode "off")

  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls))
  )

;; face for displaying code block separators
(defface py-code-block-separator-style
  '((t . (:foreground "grey75" :background "grey30" :extend t)))

  "Display style for python code block separators.")


;; eval:
;;   (setq my-python-shell-program
;;     (k8s-parse-exec-command "name=fred" "statosphere-resources" "/bin/ash"))

;; use python cell blocks
(use-package python-x
  :hook (
	 (python-mode
	  . (lambda () (highlight-lines-matching-regexp
			"^ *# %% .*$" 'py-code-block-separator-style)))
	 )
  :custom
  (python-section-delimiter "# %% ")
  :config
  (python-x-setup))

;; function for creating a terminal shell running python
(defun my-python-run-shell-in-terminal (p &optional no-move and-run)
  "Run a python shell inside a terminal buffer.

Prefix P works like in get-or-create-terminal from terminal.el."
  (interactive "P")
  (let* (
	;; default prefix for the terminal buffer name, if unset
	(terminal-buffer-name-default-prefix
	 my-python-shell-buffer-name-default-prefix)
	;; terminal name set to python shell name
	(terminal-buffer-name my-python-shell-buffer-name)
	(terminal-buffer-name (parse-vterm-buffer-name p))
	;; init commands are python-specific init commands
	(terminal-init-commands my-python-shell-init-commands)
	;; shell program needs to be parsed if running in k8s
	(my-python-shell-program
	 (progn (if (string-equal my-python-shell-program "k8s")
		    (k8s-parse-exec-command my-python-k8s-pod-label
					    my-python-k8s-pod-namespace
					    my-python-k8s-exec-command)
		  my-python-shell-program)))
	)

    ;; update python shell name in case it was set manually
    (customize-set-value
     'my-python-shell-buffer-name terminal-buffer-name)

    ;; get or create the shell
    (get-or-create-terminal
     nil my-python-shell-program nil (not no-move) and-run)

    )
  )

;; function for executing a python code block using terminal.el
(defun my-python-execute-code-block (p)
  "Execute code block, or the visual selection if in visual mode.

Prefix P works like in get-or-create-terminal from terminal.el."
  (interactive "P")
  (my-python-run-shell-in-terminal
   p
   t
   '(lambda (term-buffer)
      (comint-send-string term-buffer "%cpaste -q\n")
      (sleep-for 0 100)
      (when (evil-normal-state-p)
	(python-mark-fold-or-section))
      (evil-send-region-to-terminal term-buffer)
      (sleep-for 0 100)
      (comint-send-string term-buffer "\n--\n")
      )
   )
  )

;; add conda.el to make Org mode work nicely with virtual environments
(use-package conda
  ;; :custom
  ;; (conda-anaconda-home "/opt/conda/bin/conda")
  :config
  (setq conda-env-executables-dir "/opt/conda/bin")
  (conda-env-autoactivate-mode -1)
  ;; (setq conda-env-home-directory (expand-file-name "~/.conda"))
  )

(provide 'python)
;;; python.el ends here
