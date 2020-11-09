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
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

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
    ("RET" . nil) ("<return>" . nil))  ; unmap return key

  :config

  ;; set minimum prefix length to 1
  (setq company-minimum-prefix-length 1)

  ;; toggle popup by changing delay between 0 and 10000
  (setq company-idle-delay ide-company-popup-active-delay)
  (defun toggle-company-idle-delay ()
"Stops or starts company auto-popup feature by setting delay to a few hours or zero."
    (interactive)
    (if (featurep 'company)
    (cond ((= company-idle-delay ide-company-popup-active-delay)
	(setq company-idle-delay 10000) (company-cancel))
	(t (setq company-idle-delay ide-company-popup-active-delay)))))
  ;; bind to "C-a" in normal and insert states
  (evil-define-key '(normal insert) 'global (kbd "C-a") 'toggle-company-idle-delay)

  ;; show numbers next to completion candidates
  (setq company-show-numbers t)

  ;; wrap around
  (setq company-selection-wrap-around t)

  ;; open company popup on any command (not just when editing text)
  (setq company-begin-commands t)

  ;; ;; enable auto-completion from filesystem
  ;; (add-to-list 'company-backends 'company-files)

  ;; add yasnippet support
  (defun company-mode/backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
  Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	backends
      (append (if (consp backends)
		  backends
		(list backends))
	      '(:with company-yasnippet))))
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends))

  ;; enable
  (global-company-mode t))

;; beautified popup
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config

  ;; show dropdown even if only one candidate
  (setq company-box-show-single-candidate t)

  ;; display documentation box right away
  (setq company-box-doc-delay 0.1)
  
  )

;; Syntax and error checks

(use-package flycheck
  :init
  (global-flycheck-mode)
  )

;; use which-key for emacs function completion
(use-package which-key
  :init
  (which-key-mode))

;;; LSP support

;; basic lsp-mode
(use-package lsp-mode

  ;; try to start up for each language
  :hook ((prog-mode . lsp)
  	;; if you want which-key integration
  	(lsp-mode . lsp-enable-which-key-integration))

  :init

  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")

  ;; enable snippet support
  (setq lsp-enable-snippet t)

  ;; ;; lsp extras
  ;; (use-package lsp-ui
  ;;   :ensure t
  ;;   :config
  ;;   (setq lsp-ui-sideline-ignore-duplicate t)
  ;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  ;; lsp-ivy
  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol)

  ;; ;; which-key integration
  ;; (with-eval-after-load 'lsp-mode
  ;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  :commands lsp)


;;; other tweaks

;; smartly versioned file editing
(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  :custom
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
