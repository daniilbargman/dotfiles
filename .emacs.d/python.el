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

;; Name of shell buffer (if nil, always prompt in minibuffer)
(defcustom my-python-shell-buffer-name nil
  "Name of (the buffer containing) the desired python shell.

If the buffer by the specified name does not exist, it is created by
opening a terminal buffer in a new window and running the command
defined by my-python-shell-program.

If this variable is not set, the user will be prompted for a targat
buffer name during each attempt to open a shell or send code to it."
  :group 'my-python
  :type 'string
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
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-pod-label nil
  "Label to identify a K8S pod that should run a python shell.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-pod-namespace nil
  "K8S namespace containing the pod that should run a python shell.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :safe (lambda (_) t))

;; labels for identifying a k8s pod if python shell should be run in one
(defcustom my-python-k8s-exec-command nil
  "Shell command for the K8S pod that should launch the python shell.

If unset, it defaults to `/bin/sh' per the `k8s-parse-exec-command'
function spec.

This variable is only used if `my-python-shell-program' is `k8s'."

  :group 'my-python
  :type 'string
  :safe (lambda (_) t))

;; initialization commands to run when a python shell first starts
(defcustom my-python-shell-init-commands nil
  "Commands to initialize the python shell."

  :group 'my-python
  :type '(repeat string)
  :safe (lambda (_) t))

;; set fill column at 79
(add-hook 'python-mode-hook (lambda () (setq fill-column 79)))

;; set tab width to 4
(add-hook 'python-mode-hook
	  (lambda ()
	    (defvar evil-shift-width) (setq evil-shift-width 4)))


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
(defun my-python-run-shell-in-terminal ()
  "Run a python shell inside a terminal buffer."
  (interactive)
  (let (
       ;; terminal name set to python shell name
       (terminal-buffer-name my-python-shell-buffer-name)
	;; init commands are python-specific init commands
	(terminal-init-commands my-python-shell-init-commands)
	;; shell program needs to be parsed if running in k8s
	(my-python-shell-program
	 (progn (if (string-equal my-python-shell-program "k8s")
		    (k8s-parse-exec-command my-python-k8s-pod-label
					    my-python-k8s-pod-namespace
					    my-python-k8s-exec-command)
		  my-python-shell-program))))
    (get-or-create-terminal my-python-shell-program nil t)))

;; function for executing a python code block using terminal.el
(defun my-python-execute-code-block ()
  "Execute code block, or the visual selection if in visual mode."
  (interactive)
  (let
      (
       ;; terminal name set to python shell name
       (terminal-buffer-name my-python-shell-buffer-name)
       ;; init commands are python-specific init commands
       (terminal-init-commands my-python-shell-init-commands)
       ;; shell program needs to be parsed if running in k8s
       (my-python-shell-program
	(progn (if (string-equal my-python-shell-program "k8s")
		   (k8s-parse-exec-command my-python-k8s-pod-label
					   my-python-k8s-pod-namespace
					   my-python-k8s-exec-command)
		 my-python-shell-program))))
    (get-or-create-terminal
     my-python-shell-program nil nil
     '(lambda (term-buffer)
	  (comint-send-string term-buffer "%cpaste\n")
	  (sleep-for 0 200)
	  (when (evil-normal-state-p)
	    (python-mark-fold-or-section))
	  (evil-send-region-to-terminal term-buffer)
	  (sleep-for 0 200)
	  (comint-send-string term-buffer "--\n")
	 )
      )))

;; functions for formatting expressions inside braces
(defun my-python-format-parens ()
  "Format collections inside parens."
  (interactive)
  ;; (message (number-to-string (nth 0 (syntax-ppss))))
  ;;  )

  ;; only run the command if point is at an opening paren
  (when (looking-at-p "[({")

    ;; do not move cursor after exiting this function
    (save-excursion

      ;; originally move cursor to closing paren to extract scope
      (evil-jump-item)

      (let* (

	    ;; check current smartparens mode status
	    (current-smartparens-mode smartparens-mode)

	    ;; get matching paren position
	    (match-paren-pos (point))

	    ;; ;; save expression depth as failsafe against nested parens
	    ;; (sexp-depth (nth 0 (syntax-ppss)))

	    ;; pre-allocate list of breakpoints
	    (breakpoint-list nil)

	    ;; flag to check if using keywords not commas
	    (on-keywords nil)

	    ;; value of column indent, to infer how points should move
	    (column-offset nil)

	    ;; cumulative offset since start of iteration
	    (cumulative-offset 0)

	    ;; do not auto-fill text as filling is handled manually
	    (auto-fill-function nil)

	    )

	;; disable smartparens mode temporarily
	(smartparens-mode -1)

	;; save closing paren data
	(setq breakpoint-list
	      `((,(1- (point)) .
		 (1 ,(looking-back "^ *" (- match-paren-pos 79))))))

	;; return to opening paren and save opening paren data
	(evil-jump-item)
	(setq breakpoint-list
	      (append `((,(1+ (point)) . (1 ,(looking-at-p ".$"))))
		      breakpoint-list))

	;; move to beginning of first element inside parens
	(evil-forward-word-begin)
	(when (looking-back "[\"']" 1) (evil-backward-char))

	;; keep moving until closing paren
	(while (< (point) match-paren-pos)

	  ;; jump to end of word or expression
	  (let ((starting-point (point)))
	    (sp-forward-parallel-sexp)
	    (when (< (point) starting-point)
	      (goto-char starting-point) (evil-forward-word-begin)
	      )
	    )

	  ;; ;; if we stumbled on a string, skip over it
	  ;; (when (looking-at-p "[\"']") (sp-forward-sexp))

	  ;; if still at same depth level and looking at comma, add
	  ;; position after comma with flag 1
	  ;; (when (and (eql (nth 0 (syntax-ppss)) sexp-depth)
	  ;; 	     (looking-at-p ","))
	  (when (looking-at-p ",")
	    (setq breakpoint-list
		  (append
		   breakpoint-list
		   `((,(+ 2 (point)) . (1 ,(looking-at-p ".$")))))))

	  ;; ;; move to beginning of next word if looking at space
	  (when (looking-at-p " ")
	    (evil-forward-word-begin)
	    (when (looking-back "[\"']" 1) (evil-backward-char))
	    )
	  ;; (evil-forward-word-begin)
	  ;; (when (looking-back "[\"']" 1) (evil-backward-char))

	  ;; if still at same depth level and found break keyword or
	  ;; closing paren, save position before word with flag 0
	  (when (member (symbol-name (symbol-at-point))
			'("for" "if" "else"))  ; decided not to use "in"
	    (save-excursion
	      (evil-backward-word-end)
	      (setq breakpoint-list
		    (append
		    breakpoint-list
		    `((,(+ 2 (point)) . (0 ,(looking-at-p ".$")))))))))

	;; filter out some of the entries
	(cond
	 ;; if we only have commas and there is a comma after the
	 ;; last element, delete entry as it's already captured by
	 ;; the closing paren
	 ((seq-every-p
	   (lambda (elt) (eql (nth 1 elt) 1))
	   breakpoint-list)
	  (progn (goto-char (1+ (car (nth 1 breakpoint-list))))
		 (evil-backward-word-end)
		 (when (looking-at-p ",")
		   (setq breakpoint-list (butlast breakpoint-list)))))

	  ;; otherwise, filter all comma elements and only use keywords
	  (t
	   (setq breakpoint-list
		 (append
		  (list (nth 0 breakpoint-list) (nth 1 breakpoint-list))
		  (seq-filter
		   (lambda (elt) (eql (nth 1 elt) 0))
		   (nthcdr 2 breakpoint-list))))
	   (setq on-keywords t)))

	;; breakpoint-list
	(prin1 breakpoint-list)

	;; with a list of positions at hand, apply logic
	(cond

	 ;; if all breakpoints are newlines, compress to one line
	 ((seq-every-p (lambda (elt) (nth 2 elt)) breakpoint-list)
	  ;; delete trailing commas and join
	  (goto-char (car (nth 1 breakpoint-list)))
	  (evil-backward-word-end)
	  (when (looking-at-p ",") (evil-delete-char
				    (point) (1+ (point))))
	  (evil-join (car (nth 0 breakpoint-list))
		     (1- (car (nth 1 breakpoint-list)))))

	 ;; if all non-paren breakpoints have newlines, insert paren
	 ;; newlines and put everything in-between on same line
	 ((seq-every-p
	   (lambda (elt) (nth 2 elt)) (nthcdr 2 breakpoint-list))

	  ;; move to start of paren expression
	  (goto-char (1- (caar breakpoint-list)))

	  ;; join all paren contents on one line
	  (save-excursion
	    (let ((point-begin (point)))
	      (evil-jump-item)
	      (evil-join point-begin (point))))

	  ;; add newline after opening paren
	  (save-excursion
	    (evil-forward-char)
	    (newline-and-indent))

	  ;; add newline before closing paren and a trailing comma if
	  ;; appropriate
	  (evil-jump-item)
	  (unless (or (looking-back "," 1) on-keywords) (insert ","))
	  (newline-and-indent))

	 ;; fill to style if either a) all elements are on the same
	 ;; line, or b) all non-paren elements are on the same line, and
	 ;; the length of the line exceeds the fill column.
	 ((or
	   ;; case a) all elements are on the same line
	   (and
	    (seq-every-p
	     (lambda (elt) (not (nth 2 elt))) breakpoint-list)
	    (save-excursion
	      (progn (end-of-line) (>= (current-column) fill-column))))
	   ;; case b) all non-paren elements are on the same line
	   (and
	    (seq-every-p
	     (lambda (elt) (not (nth 2 elt)))
	     (nthcdr 2 breakpoint-list))
	    (save-excursion
	      (progn (goto-char (caar (last breakpoint-list)))
		     (end-of-line)
		     (>= (current-column) fill-column)))))

	  ;; some of the prep is slightly different for the two cases
	  (goto-char (caar breakpoint-list))
	  (cond
	  ;; case a)
	  ;; if every item is on the same line, first reformatting
	  ;; iteration should not insert line breaks next to
	  ;; parens; instead, assume that all elements will be offset
	  ;; one character right of the opening paren.
	   ((seq-every-p
	     (lambda (elt) (not (nth 2 elt))) breakpoint-list)
	    (setq column-offset (current-column)))

	   ;; case b)
	   ;; If all non-paren elements are on the same line which is
	   ;; too long, just need to save the beginning of first
	   ;; non-paren element as the column offset
	   (t
	    (evil-forward-word-begin)
	    (setq column-offset (current-column)))

	   )

	  ;; fill remaining points
	  (let* (

		 ;; relevant list of breakpoints
		 (active-breakpoints
		  (append (nthcdr 2 breakpoint-list)
			  (list (nth 1 breakpoint-list))))

		 ;; variables to store active points we're working on
		 (prev-elt nil)
		 (elt nil)
		 (next-elt nil)

		 )

	    ;; iterate over breakpoint list elements (starting from
	    ;; second one as we've ensured first one is as far left as
	    ;; possible by inserting newline after opening paren)
	    (dotimes (i (1- (length active-breakpoints)))

	      ;; extract elements
	      (setq prev-elt (nth i active-breakpoints)
		    elt (nth (1+ i) active-breakpoints)
		    next-elt (nth (+ 2 i) active-breakpoints))

	      ;; go to end of symbol to see if it breaks fill column
	      (goto-char (+ cumulative-offset
			    (or (car next-elt)
				(car elt))))

	      ;; move to end of symbol or to closing paren if at the end
	      (if next-elt (evil-forward-char) (evil-backward-word-end))

	      ;; if it goes beyond fill column, put newline after
	      ;; previous candidate if it's on the same line
	      (when (>= (current-column) fill-column)
		(unless (car (cddr prev-elt))
		  (goto-char
		   (+ cumulative-offset
		      (1- (if next-elt (car elt) (car prev-elt)))))
		  (newline-and-indent)
		  (setq cumulative-offset
			(+ cumulative-offset column-offset)))))))

	 ;; the remaining conditions cover the cases when non-paren
	 ;; elements are filled to style, so newlines need to be
	 ;; inserted for all non-paren elements.
	 (t

	  ;; move point to closing paren to avoid recalculating points
	  (goto-char (car (nth 1 breakpoint-list)))

	  ;; iterate over list moving backwards, and replace
	  (dolist (elt (reverse (nthcdr 2 breakpoint-list)))
	    (unless (car (cddr elt))
	      (goto-char (car elt))
	      (newline-and-indent)))))

	;; re-enable smartparens mode if it had been enabled before
	(smartparens-mode current-smartparens-mode)

	))))

;; ;; support for ipython
;; (use-package ipython-shell-send)


(provide 'python)
;;; python.el ends here
