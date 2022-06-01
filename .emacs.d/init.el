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


;; add to exec-path
(setq exec-path (append exec-path '("/usr/sbin" "/snap/bin")))

;; pre-compile packages as recommended by GCC Emacs branch
(setq package-native-compile t)

;; use straight.el instead of package.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))
;; (setq straight-enable-use-package-integration t)
(setq straight-fix-flycheck t)


;;; Basic look and feel:

;; set font to Fira Code
(set-frame-font "Fira Code")

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

;; interactively toggle character syntax between symbol and word
(defun toggle-syntax-entry (char)
  "Toggle syntax entry for CHAR between symbol to word."
  (interactive "c")
  (if (eq ?_ (char-syntax char))
      (progn (modify-syntax-entry char "w")
	     (message "word"))
    (progn (modify-syntax-entry char "_")
	   (message "symbol"))))

;; treat underscores as parts of a word by default
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

;; enable minibuffer history with a reasonable history length
(savehist-mode t)
(setq history-length 50)

;; re-enter files where we left off
(save-place-mode t)

;; auto-sync files that have been edited externally
(global-auto-revert-mode t)


;;; global packages

;; add magit
(use-package magit)

;; add rainbow mode
(use-package rainbow-mode
  :hook prog-mode org-mode)

;; add org mode
;; (use-package org)
(use-package org
  :straight '(org :type built-in)

  :config

  ;; prettify check boxes
  (add-hook 'org-mode-hook (lambda ()
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

  )

;; ;; NOTE: this package is not needed, plus it slows startup time
;; ;; massively starting from Emacs 28.1
;; ;;
;; ;; prettify unicode fonts
;; (use-package unicode-fonts
;;    :ensure t
;;    :config
;;     (unicode-fonts-setup))

;; prettify org-mode bullets
(use-package org-bullets
  :straight (org-bullets :type git :host github :repo "sabof/org-bullets")

  :hook (org-mode . org-bullets-mode)

  )

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
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
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


;; ;; THIS WORKS BUT DOESN'T REALLY IMPROVE THE EXPERIENCE
;; ;; ;; enable ligature fonts
;; (use-package fira-code-mode
;;   ;; :straight (:post-build 'fira-code-mode-install-fonts)
;;   :config (global-fira-code-mode))

;; ;; THIS WORKS WHEN FIRA CODE IS SET AS THE DEFAULT FONT, BUT DOESN'T
;; ;; REALLY IMPROVE THE EXPERIENCE EITHER
;; (use-package ligature

;;   :straight (ligature :type git :host github :repo "mickeynp/ligature.el")

;;   :config

;;   ;; Enable the www ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))

;;   ;; Enable ligatures in programming modes                                                           
;;   (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;; 				      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;; 				      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;; 				      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;; 				      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;; 				      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;; 				      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;; 				      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;; 				      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;; 				      "<~" "<~~" "</" "</>" "~@" "~-"
;; 				      "~>" "~~" "~~>" "%%"))

;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))

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
(load custom-file 'noerror 'nomessage)

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
(load-config-file "ide.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: DEPRECATED THIS BIT as it seems to hang with straight.el and
;; i'm not really using EAF widgets anyway.
;;
;; ;; add widget support
;; (load-config-file "widgets.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Session management:

;; reopen previous session on startup
(desktop-save-mode 1)
(defvar desktop-load-locked-desktop)
(setq desktop-load-locked-desktop t)

;; the end
(provide 'init)
;;; init.el ends here
