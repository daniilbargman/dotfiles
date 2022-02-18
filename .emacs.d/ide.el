;;; ide.el --- Common IDE features -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Daniil Bargman
;; Keywords: configuration

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

;; Common configuration for IDE-like functionality in Emacs:
;; 1. Improved minibuffer functionality (Ivy)
;; 2. Language Server Protocol integration
;;	- LSP support with lsp-mode
;;	- popup code completion with company
;;	- error checking with flycheck
;; 3. Parenthesis auto-closure and navigation
;; 4. Version control for code edits with undo-redo branches
;; 5. Highlighting of words under cursor and indentation blocks
;; 6. Improved status line


;;; Code:


;;; Global configuration group

;; group under "bind-interactive-shell"
(defgroup ide-config nil
  "Variables for tweaking the Emacs IDE configuration."
  :group 'External
  :prefix 'ide
  :version '0.1.0)

;; Delay for company popup (when mode is toggled to active)
(defcustom ide-company-popup-active-delay 0.2
  "Name of (the buffer containing) the desired interactive shell.

If the buffer by the specified name does not exist, it will typically be
created by opening an ansi-terminal in a new window and running the
command defined by bind-interactive-shell-command.

If this variable is not set, the user will be prompted for a targat
buffer name during each attempt to open a shell or send code to it."
  :group 'ide-config
  :type 'float)


;; Delay for company popup (when mode is toggled to active)
(defcustom ide-format-parens-opening-paren-alist '("(")
  "List of opening parens for which `ide/format-parens' will work.

  Point will have to be on one of the allowed opening paren symbols for
  the `ide/format-parens' to attempt its formatting logic. Otherwise
  the function will not work."
  :group 'ide-config
  :type '(repeat string)
  :local t
  :safe (lambda (_) t))

;; Delay for company popup (when mode is toggled to active)
(defcustom ide-paren-wrap-delimiters
  '(((after "&?[a-zA-Z0-9]+\\(?:-[a-zA-Z0-9]+\\)*\\>\\_>")))
  "List of lists of strings to use as delimiters inside parens.

  The function `ide/format-parens'iterates over several display options
  for parentheses, which is useful for languages like python and JS. For
  example, when the cursor is at the opening paren, this function will
  sequentially toggle the following options:

      using commas as the separator:

      a = {1, 2, 3}

      a = {1,
	   2,
	   3}

      a = {
	1, 2, 3,
      }

      a = {
	1,
	2,
	3,
      }

    using python's list comprehension keywords as separators:

    a = [x for x in range(10) if x > 5]

    a = [x
	 for x in range(10)
	 if x > 5]

    a = [
      x for x in range(10) if x > 5
    ]

    a = [
      x
      for x in range(10)
      if x > 5
    ]

  Newlines are dynamically inserted and removed inside paren pairs. The
  lists of delimiters defined in this variable are used as the guideline
  for where newlines can be inserted. The function looks for matches
  from each delimiter list in the order in which the lists are defined,
  and uses the first list from which at least one string has matched."
  :group 'ide-config
  :type '(alist :value-type ((alist :value-type (symbol string))))
  :local t
  :safe (lambda (_) t))


;;; Better minibuffer (Ivy)

;; use counsel
(use-package counsel
  :after ivy
  :config

  ;; enable globally
  (counsel-mode)

  ;; define some functions around counsel-yank-pop to make it consistent
  (defun my/counsel-paste-pop ()
    "Interactive like counsel-yank-pop, but removes previous paste if
  running after an evil-paste command (like evil-paste-pop)"
    (interactive)

    ;; if running after an evil-paste command...
    (if (memq last-command
	      '(evil-paste-after
		evil-paste-before
		evil-visual-paste))

	;; save paste location
	(let ((BEG (evil-get-marker ?\[))
	      (END (evil-get-marker ?\])))

	  ;; then visually select and run visual equivalent of this func
	  (evil-visual-select BEG END)
	  (my/counsel-yank-pop-selection))

      ;;; if not running after an evil-paste command, just run as normal
      (counsel-yank-pop))
    )

  ;; replace visual selection with selection from kill-buffer
  (defun my/counsel-yank-pop-selection ()
    "Replace visual selection with an item selected from the kill-ring."
    (interactive)
    (let* ((target-range (evil-visual-range))
	    (BEG (nth 0 target-range))
	    (END (nth 1 target-range))
	    )
      (goto-char END)
      (counsel-yank-pop)
      (evil-delete BEG END) ; type)
      ))

  )

;; use ivy
(use-package ivy
  ;; :defer 0.1
  ;; :diminish
  ;; :bind (("C-c C-r" . ivy-resume)
  ;;        ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
  ;; (evil-collection-init 'ivy)
  )

;; show additional information about buffers in the buffer window
(use-package ivy-rich
  :after ivy
  :custom

  ;; speed up buffer toggle
  (ivy-rich-project-root-cache-mode t)

  :config
  (ivy-rich-mode 1)
  ;; (setcdr (assq t ivy-format-functions-alist)
  ;; 	  #'ivy-format-function-line)
  )

;; use swiper
(use-package swiper
  :after ivy)
  ;; :bind (("C-s" . swiper)
  ;;        ("C-r" . swiper)))


;;; Code essentials

;; try out tree-sitter
(use-package tree-sitter
  :hook ((python-mode rust-mode) . tree-sitter-hl-mode)
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )


;; popup completion with company mode

(use-package company
  :bind
  (:map company-active-map
    ;; ("<tab>" . company-select-next)  ; cycle with <tab> / shift-<tab>
    ;; ("<backtab>" . company-select-previous)  ; cycle with tab / shift-tab
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-l" . company-complete-selection)  ; fill with C-l
    ("<tab>" . company-complete-selection)  ; fill with tab
    ("TAB" . company-complete-selection)  ; fill with tab
    ("RET" . nil) ("<return>" . nil))  ; unmap return key

  :config

  ;; ;; enable
  ;; (global-company-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; set minimum prefix length to 1
  (setq company-minimum-prefix-length 1)

  ;; set idle delay
  (setq company-idle-delay ide-company-popup-active-delay)

;;   ;; toggle popup by changing delay between 0 and 10000
;;   (setq company-idle-delay ide-company-popup-active-delay)
;;   (defun toggle-company-idle-delay ()
;; "Stops or starts company auto-popup feature by setting delay to a few hours or zero."
;;     (interactive)
;;     (if (featurep 'company)
;;     (cond ((= company-idle-delay ide-company-popup-active-delay)
;; 	   (setq company-idle-delay 10000)
;; 	   (company-cancel)
;; 	   (message "autocomplete disabled"))
;; 	  (t (setq company-idle-delay ide-company-popup-active-delay)
;; 	     (message "autocomplete enabled")))))

  ;; show numbers next to completion candidates
  (setq company-show-numbers t)

  ;; wrap around
  (setq company-selection-wrap-around t)

  ;; open company popup on any command in insert state, disable otherwise
  (setq company-begin-commands
	;; this works for auto-toggle but slows down emacs
	'(self-insert-command
	  evil-insert
	  evil-insert-line
	  evil-insert-newline-above
	  evil-insert-newline-below
	  evil-insert-state
	  evil-append
	  evil-append-line
	  evil-substitute
	  evil-replace-state
	  evil-change
	  evil-change-line
	  evil-change-whole-line
	  backward-delete-char-untabify)
	;; ;; run company via keypress only
	;; nil
	); t)
  ;; (add-hook 'evil-insert-state-entry-hook
  ;; 	    (lambda ()
  ;; 	      (setq company-idle-delay ide-company-popup-active-delay)))
  ;; (add-hook 'evil-insert-state-exit-hook
  ;; 	    (lambda ()
  ;; 	      (setq company-idle-delay 10000)))

  ;; ;; enable auto-completion from filesystem
  ;; enable
  ;; (add-to-list 'company-backends 'company-files)

  ;; ;; weight by frequency
  ;; (setq company-transformers '(company-sort-by-occurrence))

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
	    (and (listp backend) (member 'company-yasnippet backend)))
    backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))

  ;; add yasnippet support initially and whenever lsp-mode is enabled
  (setq company-backends
	(mapcar 'company-mode/backend-with-yas company-backends))

  ;; enable globally
  (global-company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  )


;; beautified popup
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config

  ;; show dropdown even if only one candidate
  (setq company-box-show-single-candidate t)

  ;; display documentation box right away
  (setq company-box-doc-delay 1.0)
  
  )

;; code folding with origami mode and lsp-origami (instead of hideshow)
;; NOTE: this package is way too slow for large python files, especially
;; after creating many folds with origami-toggle-all-nodes
;; (use-package origami
;;  :quelpa
;;    (origami :fetcher github :repo "gregsexton/origami.el")
;;  :config
;;    (global-origami-mode)
;;  )


;; Syntax and error checks
(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not
     fundamental-mode
     ovpn-mode
     vterm-mode
     special-mode
     messages-buffer-mode))
  )

;; use which-key for emacs function completion
(use-package which-key
  :init
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode))

;;; LSP support

;; basic lsp-mode
(use-package lsp-mode

  :hook (
  	;; if you want which-key integration
  	(lsp-mode . lsp-enable-which-key-integration))

  :custom

  (lsp-headerline-breadcrumb-enable nil) ; disable breadcrumb
  ;; (lsp-log-io t)
  ;; (lsp-print-performance t)
  ;; (lsp-server-trace t)
  (lsp-response-timeout 10)
  ;; (lsp-print-performance t)
  ;; (lsp-enable-file-watchers nil)
  (lsp-restart 'auto-restart)

  :init

  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l"
	lsp-enable-snippet t
	;; lsp-auto-configure nil
	)

  ;; lsp extras
  (use-package lsp-ui
    :custom

    ;; lsp-ui-doc
    (lsp-ui-doc-position 'at-point)
    (lsp-ui-doc-show-with-cursor nil)
    ;; (lsp-ui-doc-header t)
    (lsp-ui-doc-max-height 50)
    ;; (lsp-ui-doc-use-webkit t)

    ;; disable sideline
    (lsp-ui-sideline-enable nil)

    ;; lsp-ui-peek
    ;; (lsp-ui-peek-enable t)

    ;; :config
    ;; (lsp-ui-peek-mode)  ; disable peek mode
    ;; (lsp-ui-sideline-mode)  ; disable sideline mode
    
    )

  ;; performance optimization settings
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.5)

  ;; lsp-ivy
  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol)

  ;; lsp-treemacs
  (use-package lsp-treemacs
    :commands lsp-treemacs-sync-mode
    :config
      (lsp-treemacs-sync-mode 1))

  ;; NOTE: this throws an error
  ;; trying out code folding support with lsp mode
  ;; (use-package lsp-origami
  ;;   :quelpa
  ;;   (lsp-origami :fetcher github :repo "emacs-lsp/lsp-origami.el" )
  ;;   :commands lsp-origami-try-enable
  ;;   :hook (lsp-after-open . 'lsp-origami-try-enable))

  ;; which-key integration
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  ;; make sure to enable yasnippet support in lsp completion
  (with-eval-after-load 'lsp-mode
    (add-hook
      'lsp-completion-mode-hook
	(lambda ()
	  (setq company-backends
		(cl-remove-duplicates
		 (mapcar 'company-mode/backend-with-yas
			 company-backends)
		 :test 'equal-including-properties
		 :from-end t)))
    ))

  :commands lsp)

;;; other tweaks

;; smartly versioned file editing
(use-package undo-tree
  :custom

    ;; save history across sessions
    (undo-tree-auto-save-history t)

    ;; avoid file clutter: save all undo files in emacs directory
    (undo-tree-history-directory-alist
    '(("." . "~/.emacs.d/undo-tree")))

    ;; disable undo-redo in regions; use file-level only
    (undo-tree-enable-undo-in-region nil)

  :config

    ;; enable globally by defualt
    (global-undo-tree-mode 1)
    
  )

;; auto-insert parentheses
(use-package smartparens

  :custom

  ;; do not auto-escape quotes inside strings
  (sp-escape-quotes-after-insert nil)

  ;; ;; do not autowrap regions
  ;; (sp-autowrap-region nil)

  ;; ;; highlight parens enclosing actively edited region
  ;; (sp-show-pair-from-inside t)

  :config

  ;; add hook to indent according to context
  (defun indent-to-context (id action mode)
    "indent closing paren according to context."
    (when (or (eq action 'insert) (eq action 'wrap))
      (save-excursion
	(evil-backward-WORD-end)
	(evil-jump-item)
	(indent-according-to-mode))))

  ;; duplicate spaces and newlines inside matching pairs, unless already
  ;; inserted
  (sp-local-pair 'prog-mode "( " " )")
  (sp-local-pair 'prog-mode "(\n" "\n)"
	   :unless '(sp-point-before-eol-p sp-point-before-word-p)
	   :post-handlers '(:add indent-to-context)
	   )
  (sp-local-pair 'prog-mode "[ " " ]")
  (sp-local-pair 'prog-mode "[\n" "\n]"
	   :unless '(sp-point-before-eol-p sp-point-before-word-p)
	   :post-handlers '(:add indent-to-context)
	   )
  (sp-local-pair 'prog-mode "{ " " }")
  (sp-local-pair 'prog-mode "{\n" "\n}"
	   :unless '(sp-point-before-eol-p sp-point-before-word-p)
	   :post-handlers '(:add indent-to-context)
	   )

  ;; enable globally
  (smartparens-global-mode t)

  ;; highlight matching/mismatching parentheses
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)

  ;; load default config
  (require 'smartparens-config))


;; functions for formatting expressions inside braces
(defun ide/format-parens ()
  "Format collections inside parens."
  (interactive)
  ;; (message (number-to-string (nth 0 (syntax-ppss))))
  ;;  )

  ;; only run the command if point is at an opening paren
  (when (looking-at-p
	 (string-join
	  `("["
	    ,(string-join ide-format-parens-opening-paren-alist)
	    "]")))

    ;; do not move cursor after exiting this function
    (save-excursion

      ;; originally move cursor to closing paren to extract scope
      (evil-jump-item)

      (let* (

	     ;; save current state of smartparens modes
	     (spm smartparens-mode)
	     (spgm smartparens-global-mode)
	     (spsm smartparens-strict-mode)
	     (spgsm smartparens-global-strict-mode)

	     ;; temporarily disable fill functions; handle manually
	     (auto-fill-function nil)

	     ;; get matching paren position
	     (match-paren-pos (point))

	     ;; save expression depth as failsafe against nested parens
	     (sexp-depth (ppss-depth (syntax-ppss)))

	     ;; pre-allocate list of breakpoints
	     (breakpoint-list nil)

	    )

	;; disable smartparens modes
	(smartparens-mode -1)
	(smartparens-global-mode -1)
	(smartparens-strict-mode -1)
	(smartparens-global-strict-mode -1)
	

	;; return to opening paren
	(evil-jump-item)

	;; try searching for elements of alists containing delimiters.
	;; If found, assemble list of corresponding breakpoints
	(let (
	    ;; allocate variables for saving start and end of each
	      ;; delimeter match
	      (match-exists nil)
	      (match-start-pos nil)
	      (match-end-pos nil)
	      )

	  (setq breakpoint-list
	    (cl-loop
	    for delim-alist in ide-paren-wrap-delimiters

	    ;; loop over each element in an alist and look for match
	    if (cl-loop
		for delim in delim-alist

		;; check if match exists
		if (cl-loop
		    while t
		    if (progn
			 (setq
			  match-exists (re-search-forward
					(cadr delim)
					(- match-paren-pos 1)
					t)
			  match-start-pos (match-beginning 0)
			  match-end-pos (match-end 0)
			  )
			  match-exists
			 )

		    if (progn
			(goto-char (- match-end-pos 1))
			(let ((match-valid
			      (and
				;; check same ppss depth
				(= (ppss-depth (syntax-ppss)) sexp-depth)
				;; check that it's not inside a string
				(not (in-string-p)))))

			  ;; if match is successful, return breakpoint
			  ;; depending on whether it is a "before" or
			  ;; "after" keyword
			  (forward-char)
			  (when match-valid
			    (if (eq (car delim) 'after)
				`(,it ,(looking-at-p "$"))
			      (save-excursion
				;; (goto-char (- it (length (cadr delim))))
				(goto-char match-start-pos)
				`(,(point)
				  ,(looking-back
				    "^ *" (- match-paren-pos 200))
				  )
				)
			      )
			    )
			  )
			)

		    ;; append match to breakpoint list
		    collect it into breakpoint-list-for-one-delim
		    end
		    else return breakpoint-list-for-one-delim
		    )
		collect it into breakpoint-list
		finally return (apply 'append breakpoint-list)
		)
	    return it
	    ))
	)

	;; only continue if any breakpoints have been found
	(when breakpoint-list

	  ;; prepend closing paren data to the list of breakpoints
	  (goto-char match-paren-pos)
	  (setq breakpoint-list
		(append
		 `((,match-paren-pos
		    ,(looking-back "^ *" (- match-paren-pos 200))))
		 breakpoint-list
		 ))

	  ;; prepend opening paren data to the list of breakpoints
	  (evil-jump-item)
	  (setq breakpoint-list
		(append
		 `((
		   ,(1+ (point))
		   ,(looking-at-p ".$")
		   ))
		 breakpoint-list
		 ))

	  ;; print the breakpoint list
	  (prin1 breakpoint-list)

	  ;; reformat paren contents depending on current state
	  (cond

	    ;; if all breakpoints are newlines, compress to one line
	    ((seq-every-p 'cadr breakpoint-list)

	     ;; simply join between opening paren and closing paren
	     (evil-join (1- (caar breakpoint-list)) match-paren-pos)

	     )

	    ;; if all non-paren breakpoints are newlines, insert paren
	    ;; newlines and put everything in-between on same line
	    ((seq-every-p 'cadr (nthcdr 2 breakpoint-list))

	     ;; join everything onto one line
	     (evil-join (caar breakpoint-list) match-paren-pos)

	     ;; go to opening paren and insert newline
	     (goto-char (caar breakpoint-list))
	     (newline-and-indent)

	     ;; go to closing paren and insert newline
	     (goto-char (1- (caar breakpoint-list)))
	     (evil-jump-item)
	     (newline-and-indent)

	     )

	    ;; if at least one of the non-paren breakpoints is a newline
	    ;; (but not all of them), then we have already filled to
	    ;; style and so newlines need to be inserted everywhere
	    ((seq-some 'cadr (nthcdr 2 breakpoint-list))

	     ;; start from last element, and move back
	     (cl-loop for bp in (reverse (nthcdr 2 breakpoint-list))
		      unless (cadr bp) do
		      (goto-char (car bp)) (newline-and-indent))

	     )

	    ;; remaining case: if none of the non-paren breakpoints have
	    ;; newlines, we fill to style if the line is too long, or
	    ;; insert newlines everywhere if line length is within
	    ;; specified limits
	    (t

	     ;; we only care about non-paren elements from now on
	     (let ((breakpoint-list (nthcdr 2 breakpoint-list)))

	       ;; go to end of line and check if too long
	       (goto-char (caar breakpoint-list))
	       (end-of-line)

	       (cond

		;; if line is too long, apply fill paragraph
		((>= (current-column) fill-column)

		 ;; pre-allocate variables for tracking indent
		 (let (
		       (cumulative-offset 0)
		       (bplist (mapcar 'car breakpoint-list))
		       )
		   ;; loop and insert newlines ahead of breakpoints that
		   ;; break beyond fill column setting
		   (cl-loop
		    for bp below (length bplist) do

		    ;; go to breakpoint following this one, or to end of
		    ;; line if we're at the last breakpoint
		    (let (
			  (this-bp (nth bp bplist))
			  (next-bp (nth (1+ bp) bplist))
			  )
		      (if next-bp
			  (goto-char (+ cumulative-offset next-bp))
			(end-of-line))

		      ;; check if the column is beyond fill-column
		      (when (>= (current-column) fill-column)

			;; if yes, insert newline at this breakpoint
			(goto-char (+ cumulative-offset this-bp))
			(newline-and-indent)
			(setq cumulative-offset (- (point) this-bp 1))

			)
		      )
		    )
		   )
		 )

	        ;; otherwise, insert newlines everywhere
		(t

		 (cl-loop for bp in (reverse breakpoint-list) do
			  (goto-char (car bp)) (newline-and-indent))
		 )
		)
	       )
	     )
	    )
	  )

	;; revert smartparens mode to original state
	(when spm (smartparens-mode spm))
	(when spgm (smartparens-global-mode spgm))
	(when spsm (smartparens-strict-mode spsm))
	(when spgsm (smartparens-global-strict-mode spgsm))
	)
      )
    )
  )


;; highlighting of words/symbols under cursor
(use-package highlight-symbol

  ;; enter highlight nav mode when entering normal state in evil
  :hook (evil-normal-state-entry . highlight-symbol-nav-mode)

  :config

  ;; highlight symbols under cursor
  (highlight-symbol-mode t)

  ;; highlight after 0.4 seconds
  (setq highlight-symbol-idle-delay 0.4)

  ;; ;; not sure what the variable below actually does
  ;; (setq highlight-symbol-on-navigation-p t)

  )

;; highlight indent lines
;; (use-package highlight-indentation)
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive nil)
  ;; (highlight-indent-guides-delay 0.4)
  :config
  )


;; better modeline
(use-package telephone-line
  :config
  (telephone-line-mode 1))


;; done
(provide 'ide.el)
;;; ide.el ends here
