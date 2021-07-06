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

;; set fill column at 79
(add-hook 'js-mode-hook (lambda () (setq fill-column 79)))

;;; add svelte support
(use-package svelte-mode
  ;;; disabling this hook as it tends to mess up tab-bar tabs
  ;; :hook (svelte-mode . lsp-deferred)
  :custom
  (svelte-basic-offset 4)
  (svelte-display-submodule-name t)

  :config

  ;; associate .svelte files with svelte-mode
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

  ;; ;; add svelte to LSP language ID index
  ;; (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))

  )

;; ;; install lsp server for tailwind-css
;; (use-package lsp-tailwindcss

;;   :custom

;;   ;; make sure tailwindcss is used as an add-on server
;;   (lsp-tailwindcss-add-on-mode t)

;;   :config

;;   ;; add tailwindcss to LSP language ID index
;;   (add-to-list 'lsp-language-id-configuration '(svelte-mode . "tailwindcss"))
;;   (add-to-list 'lsp-language-id-configuration '(js-mode . "tailwindcss"))
;;   (add-to-list 'lsp-language-id-configuration '(css-mode . "tailwindcss"))
;;   (add-to-list 'lsp-language-id-configuration '(html-mode . "tailwindcss"))

;;   )

;; ;; make sure flycheck works with multiple major modes enabled
;; (with-eval-after-load 'lsp-mode
;;   (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode svelte-mode)))


(provide 'web)

;;; web.el ends here
