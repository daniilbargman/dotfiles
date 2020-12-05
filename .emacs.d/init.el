;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: Daniil Bargman
;;; daniil.bargman@gmail.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the main interface file that accomplishes two tasks:
;; 1. Defining basic configuration for Emacs builtins
;; 2. Loading modular extension files

;;; Code:


;;; Basic look and feel:

;; remove distractions
(setq inhibit-startup-message t)  ; no startup screen
(tool-bar-mode -1)                ; no toolbar
(menu-bar-mode -1)                ; no menubar
(scroll-bar-mode -1)              ; no scrollbar
;; (mouse-avoidance-mode 'banish)    ; move away mouse pointer

;; add line numbers and highlight current line in minibuffer
(global-display-line-numbers-mode t)
(line-number-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; don't blink the cursor
(blink-cursor-mode 0)

;; make yes/no questions y/n questions
(fset 'yes-or-no-p 'y-or-n-p)

;; make translucent (<active opacity> . <inactive opacity>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))


;;; Basic settings for working with text

;; wrap text at 72 characters by default
(setq-default fill-column 72)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; enable text folding via hideshow
(load-library "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'text-mode-hook 'hs-minor-mode)

;; don't use system clipboard
(setq select-enable-clipboard nil)


;;; Package management:

;; initialize package manager with the right repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; NOTE: this is no longer required with Emacs 27.1
;; set this to make sure packages are installed from ELPA
; (defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; install "use-package" macro
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; always "ensure" that package is installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; install quelpa for getting packages directly from github
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; add org mode
(use-package org)


;;; Config file management:

;; constant referring to the config file directory
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

;; function for loading config files
(defun load-config-file (file)
  "Load FILE in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

;; use custom file for auto-generated code and make sure it's loaded
(setq custom-file (concat user-init-dir "custom-file.el"))
(when (file-exists-p custom-file) (load-config-file custom-file))

;;; Load configuration files

;; run startup scripts
(load-config-file "startup-scripts.el")

;; Vim keybindings
(load-config-file "evil.el")

;; interactive terminal shell
(load-config-file "bind-terminal-shell.el")

;; openvpn client
(load-config-file "openvpn.el")

;; code and text snippets
(load-config-file "snippets.el")

;; language-agnostic IDE features
(load-config-file "ide-base.el")

;; window and tab management
(load-config-file "windows-and-tabs.el")

;; customizations for Org mode
(load-config-file "org.el")

;; Kubernetes integration
(load-config-file "k8s.el")

;; ELisp configuration
(load-config-file "elisp.el")

;; bash configuration
(load-config-file "bash.el")

;; Python configuration
(load-config-file "python.el")

;; YAML integration
(load-config-file "yaml.el")

;; ;; use material theme
;; (use-package material-theme
;;   :config
;;   (load-theme 'material t)
;;   )

;; use sanityinc theme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t) ; night / bright
  )

;;; Session management:

;; reopen previous session on startup
(desktop-save-mode 1)
(defvar desktop-load-locked-desktop)
(setq desktop-load-locked-desktop t)

;; the end
(provide 'init)
;;; init.el ends here
