;;; ide-base.el --- Common IDE features -*- lexical-binding: t; -*-

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

;;; Better minibuffer (Ivy)

;; use counsel
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

;; use ivy
(use-package ivy
  :ensure t
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
  (evil-collection-init 'ivy)
  )

;; show additional information about buffers in the buffer window
(use-package ivy-rich
  :after ivy
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


;; popup completion with company mode

(use-package company
  :bind
  (:map company-active-map
    ;; ("<tab>" . company-select-next)  ; cycle with <tab> / shift-<tab>
    ;; ("<backtab>" . company-select-previous)  ; cycle with tab / shift-tab
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-l" . company-complete-selection)  ; fill with C-l
    ("<tab>" . yas-expand)  ; expand snippets with tab
    ("TAB" . yas-expand)  ; expand snippets with tab
    ("RET" . nil) ("<return>" . nil))  ; unmap return key

  :config

  ;; enable
  (global-company-mode)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; set minimum prefix length to 1
  (setq company-minimum-prefix-length 1)

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
	  backward-delete-char-untabify)); t)
  ;; (add-hook 'evil-insert-state-entry-hook
  ;; 	    (lambda ()
  ;; 	      (setq company-idle-delay ide-company-popup-active-delay)))
  ;; (add-hook 'evil-insert-state-exit-hook
  ;; 	    (lambda ()
  ;; 	      (setq company-idle-delay 10000)))

  ;; ;; enable auto-completion from filesystem
  ;; enable
  ;; (add-to-list 'company-backends 'company-files)

  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence))

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
  ;; (add-hook 'after-init-hook 'global-company-mode)
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
;;    (lsp-origami :fetcher github :repo "gregsexton/origami.el")
;;  :config
;;    (global-origami-mode)
;;  )


;; Syntax and error checks
(use-package flycheck
  :init
  (global-flycheck-mode)
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
  (lsp-response-timeout 30)
  ;; (lsp-print-performance t)
  ;; (lsp-enable-file-watchers nil)

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

    ;; lsp-ui-peek
    ;; (lsp-ui-peek-enable t)

    :config
    (lsp-ui-peek-mode)
    
    )

  ;; performance optimization settings
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

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
    (undo-tree-auto-save-history t)
    ;; avoid file clutter: save all undo files in emacs directory
    (undo-tree-history-directory-alist
    '(("." . "~/.emacs.d/undo-tree")))
  :config
    (global-undo-tree-mode 1))

;; auto-insert parentheses
(use-package smartparens
  :config

  ;; enable globally
  (smartparens-global-mode t)

  ;; highlight matching/mismatching parentheses
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)

  ;; load default config
  (require 'smartparens-config))

;; highlighting of words/symbols under cursor
(use-package highlight-symbol

  ;; enter highlight nav mode when entering normal state in evil
  :hook (evil-normal-state-entry . highlight-symbol-nav-mode)

  :config

  ;; highlight symbols under cursor
  (highlight-symbol-mode t)

  ;; highlight after 0.5 seconds
  (setq highlight-symbol-idle-delay 0.5)

  ;; ;; not sure what the variable below actually does
  ;; (setq highlight-symbol-on-navigation-p t)

  )

;; highlight indent lines
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  )


;; highlight wrap column (note: should be built in as of emacs 27)
(use-package fill-column-indicator
  :hook (prog-mode . fci-mode)
  :config
  (setq fci-rule-width 2)
  ;; (setq fci-rule-color "grey10")
  )

;; better modeline
(use-package telephone-line
  :config
  (telephone-line-mode 1))


;; done
(provide 'ide-common.el)
;;; ide-base.el ends here
