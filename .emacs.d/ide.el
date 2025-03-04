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
(defcustom ide-completion-popup-active-delay 0.2
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
  "List of opening parens for which `dbargman/format-parens' will work.

  Point will have to be on one of the allowed opening paren symbols for
  the `dbargman/format-parens' to attempt its formatting logic.
  Otherwise the function will not work."
  :group 'ide-config
  :type '(repeat string)
  :local t
  :safe (lambda (_) t))

;; Delay for company popup (when mode is toggled to active)
(defcustom ide-paren-wrap-delimiters
  '(((after "&?[a-zA-Z0-9]+\\(?:-[a-zA-Z0-9]+\\)*\\>\\_>")))
  "List of lists of strings to use as delimiters inside parens.

  The function `dbargman/format-parens'iterates over several display
  options for parentheses, which is useful for languages like python and
  JS. For example, when the cursor is at the opening paren, this
  function will sequentially toggle the following options:

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


;; minibuffer
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :config
  (vertico-mode)
  :custom
  (enable-recursive-minibuffers t)
  (vertico-cycle t)
  )

;; custom/smarter completion ordering
(use-package prescient
  :custom
  (prescient-persist-mode t)
  (prescient-sort-length-enable nil)
  (prescient-sort-full-matches-first t)
  (prescient-filter-method '(literal-prefix))
  )
(use-package vertico-prescient
  :custom
  (vertico-prescient-override-sorting t)
  :config (vertico-prescient-mode)

  ;; use regexp filtering in the minibuffer only
  :hook ((minibuffer-mode . (lambda () (prescient-toggle-regexp nil))))

  )


;; like counsel
(use-package consult

  :config

  ;; use consult for region completion (should enable completions for
  ;; evil's ":" prompt)
  (setq completion-in-region-function 'consult-completion-in-region)

  ;; when running yank-pop in visual mode, remove selected text prior.
  (defun dbargman/consult-replace-selection-from-kill-ring ()
    "Replace visual selection with an item selected from the kill-ring."
    (interactive)

    ;; advice: when command is being cancelled, simply reinsert the text
    ;; that was just removed.
    (defun undo-after-abort ()
      "reinsert old text when canceling consult-yank-replace command"
      (interactive)

      ;; remove the advice before doing anything else
      (advice-remove 'abort-minibuffers 'undo-after-abort)

      ;; move to beginning of line in case there is existing manual
      ;; input in the minibuffer
      (move-beginning-of-line nil)

      ;; reinsert last killed text (this will be the text that is being
      ;; replaced, except in some edge cases where something was killed
      ;; inside the minibuffer while the kill ring was being browsed)
      (yank)

      ;; we just pasted the text at the very beginning of the line. if
      ;; the line is not empty, delete the rest of it.
      (unless (= (point) (save-excursion (end-of-visual-line) (point)))
	(kill-line)
	)

      ;; select what is left in the manual input field (this will be the
      ;; text that was just deleted as part of the replace function)
      (vertico-exit-input)

      )
    (advice-add 'abort-minibuffers :override 'undo-after-abort)

    ;; delete current contents of selection
    (let* ((target-range (evil-visual-range))
	    (BEG (nth 0 target-range))
	    (END (nth 1 target-range))
	    )
      (evil-normal-state)
      (goto-char END)
      (evil-delete BEG END)

      ;; select replacement from kill ring interactively
      (funcall-interactively 'consult-yank-replace
			     (consult--read-from-kill-ring))
      )
    )


  ;; run consult searches on symbol at point
  (defun dbargman/consult-line-symbol-at-point ()
    "Select symbol at point and run consult-line."
    (interactive)
    (funcall 'consult-line (symbol-name (symbol-at-point)))
    )
  (defun dbargman/consult-rg-symbol-at-point ()
    "Select symbol at point and run consult-ripgrep."
    (interactive)
    (consult-ripgrep nil (symbol-name (symbol-at-point)))
    )

  ;; ;; when searching for visual selection with consult-line, do not move point
  ;; (defun start-with-current-line (lines)
  ;;   (cl-loop
  ;;    with current = (line-number-at-pos (point)
  ;; 					consult-line-numbers-widen)
  ;;   for (line . after) on (cdr lines)
  ;;   while (< (cdr (get-text-property 0 'consult-location line)) current)
  ;;   collect line into before
  ;;   finally (return (cons (car lines) (append after before)))))

  ;; (advice-add 'consult--line-candidates
  ;; 	      :filter-return #'start-with-current-line)


  :custom
  ;; search text from the top of the buffer
  (consult-line-start-from-top nil)
  )

;; actions on objects
(use-package embark
  :straight '(embark :files (:defaults "*") :includes embark-consult)

  :init

  ;; use embark window for key help
  (setq prefix-help-command #'embark-prefix-help-command)


  :config

  ;;; embark-which-key integration
  ;;; borrowed from: https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  )
;; (straight-use-package 'embark-consult)


;; Enable richer annotations using the Marginalia package
(use-package marginalia

  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; add icon support in marginalia
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; fast search across multiple files
(use-package rg)


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

  ;; set minimum prefix length to 3 for performance reasons
  (setq company-minimum-prefix-length 2)

  ;; set idle delay
  (setq company-idle-delay ide-completion-popup-active-delay)

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

  ;; configure dabbrev backend to be more helpful and less slow
  (setq
   company-dabbrev-downcase nil
   company-dabbrev-code-everywhere t
   ;; company-dabbrev-time-limit 0.05
   ;; company-dabbrev-code-time-limit 0.05
   )

  ;; open company popup on any command in insert state, disable otherwise
  (setq company-begin-commands
	;; this works for auto-toggle but slows down emacs
	'(self-insert-command
	  org-self-insert-command
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

(use-package company-prescient
  :config

  ;; ;; change filter method for company completions to avoid suggesting
  ;; ;; candidates that match in the middle
  ;; (advice-add
  ;;  'company-complete :before
  ;;  #'(lambda () (setq prescient-filter-method '(literal-prefix initialism)))
  ;;  )

  ;; enable
  (company-prescient-mode)

  ;; ;; ;; don't suggest completions that match in the middle
  ;; :hook ((company-completion-started . prescient-toggle-regexp))

  )

;; ;; alternative to company: corfu
;; (use-package corfu
;;   :straight (corfu :files (:defaults "extensions/*")
;;                      :includes (corfu-echo
;;                                 ;; corfu-info
;;                                 corfu-popupinfo
;;                                 ;; corfu-quick
;;                                 ))


;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   ;; (corfu-quit-at-boundary nil)
;;   (corfu-quit-no-match t)
;;   (corfu-preview-current nil)
;;   (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-scroll-margin 2)        ;; Use scroll margin

;;   (corfu-auto-prefix 1)
;;   (corfu-auto-delay ide-completion-popup-active-delay)

;;   (corfu-popupinfo-delay '(1.0 . 1.0))

;;   (:map corfu-map
;;     ;; ("<tab>" . company-select-next)  ; cycle with <tab> / shift-<tab>
;;     ;; ("<backtab>" . company-select-previous)  ; cycle with tab / shift-tab
;;     ("C-n" . corfu-next)
;;     ("C-p" . corfu-previous)
;;     ("C-l" . corfu-complete)  ; fill with C-l
;;     ("<tab>" . corfu-insert)  ; fill with tab
;;     ("TAB" . corfu-insert)  ; fill with tab
;;     ("RET" . nil) ("<return>" . nil)  ; unmap return key
;;     ("M-g" . corfu-info-location)
;;     ("M-h" . corfu-info-documentation)
;;     ("M-SPC" . corfu-insert-separator)
;;     ("C-g" . corfu-quit)
;;     )

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; ;; Recommended: Enable Corfu globally.
;;   ;; ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; ;; See also `corfu-excluded-modes'.
;;   :init
;;   (global-corfu-mode)

;;   :config
;;   (corfu-popupinfo-mode)

;;   )

;; ;; Add completion functions
;; (use-package cape
;;   ;; Bind dedicated completion commands
;;   ;; Alternative prefix keys: C-c p, M-p, M-+, ...
;;   ;; :bind (("C-c p p" . completion-at-point) ;; capf
;;   ;;        ("C-c p t" . complete-tag)        ;; etags
;;   ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ("C-c p k" . cape-keyword)
;;   ;;        ("C-c p s" . cape-symbol)
;;   ;;        ("C-c p a" . cape-abbrev)
;;   ;;        ("C-c p i" . cape-ispell)
;;   ;;        ("C-c p l" . cape-line)
;;   ;;        ("C-c p w" . cape-dict)
;;   ;;        ("C-c p \\" . cape-tex)
;;   ;;        ("C-c p _" . cape-tex)
;;   ;;        ("C-c p ^" . cape-tex)
;;   ;;        ("C-c p &" . cape-sgml)
;;   ;;        ("C-c p r" . cape-rfc1345))
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;; ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;   )

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   (kind-icon-extra-space t)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; easy text navigation (easymotion-style)
(use-package avy
  :custom
  (avy-timeout-seconds 0.2)
  ;; (avy-background t)
  ;; (avy-highlight-first t)
  ;; (avy-indent-line-overlay t)
  :config
  (custom-set-faces
   '(avy-lead-face
     ((t (:foreground "red" :background "gray10")))
    )
   '(avy-lead-face-0
     ((t (:foreground "red" :background "gray10")))
    )
   '(avy-lead-face-2
     ((t (:foreground "red" :background "gray10")))
    )
   )
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
  (which-key-mode)
  )

;;; END NOTE

;;; LSP support

;; basic lsp-mode
(use-package lsp-mode

  :hook (
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)

	 ;; (lsp-mode
	 ;;  . (lambda ()
	 ;;      (setq-local
	 ;;       company-backends
	 ;;       '((:separate company-yasnippet company-capf)))
         ;;      (setq
	 ;;       completion-styles '(prescient)
	 ;;       gnus-completion-styles '(prescient)
	 ;;       )
	 ;;      )
	 ;;  )

	 ;; (lsp-mode
	 ;;  . (lambda ()
	 ;;      (setq-local
	 ;;       company-backends
	 ;;       (cl-remove-duplicates
	 ;; 	(append
	 ;; 	 `(,(company-mode/backend-with-yas 'company-capf))
	 ;; 	 company-backends
	 ;; 	 )
	 ;; 	:test 'equal-including-properties
	 ;; 	:from-end t
	 ;; 	)
	 ;;       )
	 ;;      )
	 ;;  )

	 )

  :custom

  ;; (lsp-headerline-breadcrumb-enable nil) ; disable breadcrumb
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; (lsp-log-io t)
  ;; (lsp-print-performance t)
  ;; (lsp-server-trace t)
  (lsp-response-timeout 10)
  ;; (lsp-print-performance t)
  ;; (lsp-enable-file-watchers nil)
  (lsp-restart 'auto-restart)		; 'interactive)

  ;; do not show full documentation in the minibuffer popup - just the
  ;; function signature
  (lsp-signature-render-documentation nil)

  ;; try disabling this so LSP doesn't automatically try to connect to
  ;; servers with wrong roots on startup
  (lsp-auto-select-workspace nil)

  ;; set 'lsp-completion-provider' to :none as described here:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3173
  (lsp-completion-provider :none)
  ;; (lsp-completion-provider :capf)

  ;; disable completion caching. we have prescient for that so shouldn't
  ;; be an issue
  (lsp-completion-no-cache t)

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
    (lsp-ui-doc-position 'top)		; 'at-point
    (lsp-ui-doc-show-with-cursor nil)
    ;; (lsp-ui-doc-header t)
    (lsp-ui-doc-max-height 50)
    ;; (lsp-ui-doc-use-webkit t)

    ;; disable sideline
    (lsp-ui-sideline-enable nil)

    ;; lsp-ui-peek
    ;; (lsp-ui-peek-enable t)

    :config


    ;; disable fuzzy matching
    (advice-add #'lsp-completion--regex-fuz :override #'identity)

    ;; ;; NOTE: manual hack no longer required as of the latest update
    ;; ;;
    ;; ;; add hook for disabling tab-bar in child frame
    ;; (setq lsp-ui-doc-frame-hook
    ;; 	  #'(lambda (frame window) (toggle-frame-tab-bar frame))
    ;; 	  )

    ;; (lsp-ui-peek-mode)  ; disable peek mode
    ;; (lsp-ui-sideline-mode)  ; disable sideline mode
    )

  ;; performance optimization settings
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.5)

  ;; ;; lsp-ivy
  ;; (use-package lsp-ivy
  ;;   :commands lsp-ivy-workspace-symbol)

  ;; lsp-treemacs
  (use-package lsp-treemacs
    :commands lsp-treemacs-sync-mode
    ;; :config

    ;; ;; note: this may be useful for some setups, but it's not a great
    ;; ;; idea for my current project setup for statOsphere where one of
    ;; ;; the Treemacs workspaces points to a global project root. This
    ;; ;; forces LSP to try to connect to the global root (which doesn't
    ;; ;; have any actual project roots in it) and scan a bunch
    ;; ;; of files which should not be scanned.
    ;; (lsp-treemacs-sync-mode 1)
    )

  ;; add consult-lsp
  (use-package consult-lsp
    :straight
    (consult-lsp :type git
		 :host github
		 :repo "gagbo/consult-lsp"
		 )
    )
  

  ;; NOTE: this throws an error
  ;; trying out code folding support with lsp mode
  ;; (use-package lsp-origami
  ;;   :quelpa
  ;;   (lsp-origami :fetcher github :repo "emacs-lsp/lsp-origami.el" )
  ;;   :commands lsp-origami-try-enable
  ;;   :hook (lsp-after-open . 'lsp-origami-try-enable))

;;; NOTE: TRYING TO REPLACE WITH HACK BY SETTING
;;; 'lsp-completion-provider' to :none as described in
;;; https://github.com/emacs-lsp/lsp-mode/issues/3173

  ;; ;; make sure to enable yasnippet support in lsp completion
  ;; (with-eval-after-load 'lsp-mode
  ;;   (add-hook
  ;;     'lsp-completion-mode-hook
  ;; 	(lambda ()
  ;; 	  (setq company-backends
  ;; 		(cl-remove-duplicates
  ;; 		 (mapcar 'company-mode/backend-with-yas
  ;; 			 company-backends)
  ;; 		 :test 'equal-including-properties
  ;; 		 :from-end t)
  ;; 		)
  ;; 	  )
  ;;   ))

  ;; instead of the above, simply make sure company-capf with yasnippet
  ;; is on top in lsp-enabled buffers

  ;;; END NOTE


  :commands lsp)
 

;; retire undo-tree in favour of undo-fu, undo-fu-session, and vundo
(use-package undo-fu
  :custom
  (undo-limit 6710886400) ;; 64mb.
  (undo-strong-limit 100663296) ;; 96mb.
  (undo-outer-limit 1006632960) ;; 960mb.
  )
(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode)
  )
(use-package vundo
  :custom
  (vundo-window-side 'top)
  (vundo-roll-back-on-quit nil)
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-window-max-height 25)
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

  ;; pairs for LaTeX
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\[ " " \\]")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "`" "'")
  (sp-local-pair 'org-mode "\\left\\{" "\\right\\}")
  (sp-local-pair 'org-mode "\\left\\{ " " \\right\\}")
  (sp-local-pair 'org-mode "\\left(" "\\right)")
  (sp-local-pair 'org-mode "\\left( " " \\right)")
  (sp-local-pair 'org-mode "\\left[" "\\right]")
  (sp-local-pair 'org-mode "\\left[ " " \\right]")

  ;; enable globally
  (smartparens-global-mode t)

  ;; highlight matching/mismatching parentheses
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)

  ;; load default config
  (require 'smartparens-config))


;; functions for formatting expressions inside braces
(defun dbargman/format-parens ()
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
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive nil)
  (highlight-indent-guides-auto-character-face-perc 20)
  ;; (highlight-indent-guides-delay 0.4)
  :config
  )

;; better modeline
(use-package telephone-line
  :config
  (telephone-line-mode 1))

;; highlight TODO-style keywords
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
      '(("TODO"   . "#FF7500")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
  :config
  (global-hl-todo-mode))

;; done
(provide 'ide.el)
;;; ide.el ends here
