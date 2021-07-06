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


;; pre-compile packages as recommended by GCC Emacs branch
(setq package-native-compile t)

;;; Basic look and feel:

;; remove distractions
(setq inhibit-startup-message t)  ; no startup screen
(tool-bar-mode -1)                ; no toolbar
(menu-bar-mode -1)                ; no menubar
(scroll-bar-mode -1)              ; no scrollbar
;; (mouse-avoidance-mode 'banish)    ; move away mouse pointer

;; disable dialog box and kill-process confirmations
(setq use-dialog-box nil)
(setq confirm-kill-processes nil)

;; shorten delay before killing emacs to 1 second
(setq timp-kill-emacs-close-thread-delay 1)

;; add line numbers and highlight current line in minibuffer
(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; don't blink the cursor
(blink-cursor-mode 0)

;; make yes/no questions y/n questions
(fset 'yes-or-no-p 'y-or-n-p)

;; make translucent (<active opacity> . <inactive opacity>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))


;;; Basic settings for working with text

;; treat underscores as parts of a word
(modify-syntax-entry ?_ "w")

;; do not use double spaces to delimit sentences
(setq sentence-end-double-space nil)

;; wrap text at 72 characters by default
(setq-default fill-column 72)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; enable built-in fill-column indicator
(global-display-fill-column-indicator-mode)

;; enable text folding via hideshow
(load-library "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; don't use system clipboard
(setq select-enable-clipboard nil)

;; don't auto-save files (undo-tree will do this for us)
(setq auto-save-default nil)

;;; Package management:

;; initialize package manager with the right repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; NOTE: this is no longer required with Emacs 27.1
;; set this to make sure packages are installed from ELPA
(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; update packages asynchronously on a daily basis
(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config (spu-package-upgrade-daily))

;; add rainbow mode
(use-package rainbow-mode
  :hook prog-mode org-mode)

;; add org mode
(use-package org)


;;; Colour theme management

;; ;; use solarized theme
;; (use-package solarized-theme
;;   :custom
;;   (solarized-distinct-fringe-background t)
;;   :config
;;   ;; wombat color-theme with misc face definition
;;   (solarized-create-theme-file-with-palette 'dark 'solarized-wombat-dark
;;     '("#2a2a29" "#f6f3e8"
;;       "#e5c06d" "#ddaa6f" "#ffb4ac" "#e5786d" "#834c98" "#a4b5e6" "#7ec98f" "#8ac6f2")
;;     '((custom-theme-set-faces
;;       theme-name
;;       `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
;;       `(highlight ((,class (:background ,violet))))
;;       `(font-lock-builtin-face ((,class (:foreground ,magenta))))
;;       `(font-lock-constant-face ((,class (:foreground ,blue))))
;;       `(font-lock-comment-face ((,class (:foreground ,base00))))
;;       `(mode-line
;; 	((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
;;       `(mode-line-inactive
;; 	((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
;;       `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
;;       `(minibuffer-prompt ((,class (:foreground ,base1))))
;;       `(vertical-border ((,class (:foreground ,base03)))))))

;;   ;; don't change the size of org-mode headlines
;;   (setq solarized-scale-org-headlines nil)
;;   (load-theme 'solarized-wombat-dark t)
;;   )

;; ;; use sanityinc theme
;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties t) ; night / bright
;;   )

;; ;; use material theme
;; (use-package material-theme
;;   :config
;;   (load-theme 'material t)
;;   )

;; ;; use zenburn theme
;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t)
;;   )

;; ;; use gruvbox theme
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox t)
;;   )

;; ;; use atom-one-dark theme
;; (use-package atom-one-dark-theme
;;   :config
;;   (load-theme 'atom-one-dark t)
;;   )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


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

;; centralized keybinding configuration
(load-config-file "keybindings.el")

;; Vim keybindings
(load-config-file "evil.el")

;; interactive terminal shell
(load-config-file "terminal.el")

;; code and text snippets
(load-config-file "snippets.el")

;; language-agnostic IDE features
(load-config-file "ide-base.el")

;; window and tab management
(load-config-file "layout.el")

;; ELisp configuration
(load-config-file "elisp.el")

;; customizations for Org mode
(load-config-file "org.el")

;; customizations for markdown mode
(load-config-file "markdown.el")

;; YAML integration
(load-config-file "yaml.el")

;; Dockerfile integration
(load-config-file "dockerfile.el")

;; Kubernetes integration
(load-config-file "k8s.el")

;; bash configuration
(load-config-file "bash.el")

;; Python configuration
(load-config-file "python.el")

;; Python configuration
(load-config-file "web.el")

;; email client
(load-config-file "email.el")

;; openvpn client
(load-config-file "openvpn.el")

;; add widget support
(load-config-file "widgets.el")

;;; Session management:

;; reopen previous session on startup
(desktop-save-mode 1)
(defvar desktop-load-locked-desktop)
(setq desktop-load-locked-desktop t)

;; the end
(provide 'init)
;;; init.el ends here
