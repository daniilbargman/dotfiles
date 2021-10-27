;;; web.el --- web stack support for Emacs -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: convenience


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; support for web dev

;;; Code:

;; enable lsp in JavaScript mode
(add-hook 'js-mode-hook 'lsp-deferred)
;; (add-hook 'js-mode-hook 'tree-sitter-hl-mode)

;; set defaults
(add-hook
 'js-mode-hook
 (lambda ()
   (setq fill-column 79
	 ide-format-parens-opening-paren-alist '("(" "[" "{")
	 ide-paren-wrap-delimiters
	 '(((after ";"))
	   ((after ",")))
	 )
   )
 )

;;; add svelte support
(use-package svelte-mode
  ;;; enable LSP for svelte-mode
  :hook (svelte-mode . lsp-deferred)

  :init

  ;; set defaults for paren formatting and other things
  (add-hook 'svelte-mode-hook
	    (lambda ()
	      (setq fill-column 79
		    ide-format-parens-opening-paren-alist '("(" "[" "{")
		    ide-paren-wrap-delimiters
		    '(((after ";"))
		      ((after ",")))
		    )
	      )
	    )

  :custom
  (svelte-basic-offset 4)
  (svelte-display-submodule-name t)
  (lsp-svelte-plugin-typescript-enable nil)

  ;; :config
  ;; associate .svelte files with svelte-mode
  ;; (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

  )


(provide 'web)

;;; web.el ends here
