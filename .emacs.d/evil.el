;;; evil.el --- Vim integration -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <daniilbargman@daniilbargman-xps>
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

;; Vim integration via the evil-mode suite

;;; Code:

;; add vim keybindings
(use-package evil
  :after (evil-leader undo-tree smartparens)
  :init
  (setq evil-default-state 'normal)
  (setq evil-want-C-u-scroll t)  ; this breaks if evil-leader package is added
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-search-wrap nil)  ; don't stop search at buffer end

  :config (with-no-warnings


  ;;; Global settings

  ;; use evil mode by default
  (evil-mode)

  ;; set undo system via undo-tree
  (evil-set-undo-system 'undo-tree)

  ;; open terminal shells in emacs state
  (evil-set-initial-state 'term-mode 'emacs)

  ))

;; surround.vim
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; vim-commentary
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; compatibility package for use in other modes (don't enable by default)
(use-package evil-collection
  :after evil)

(provide 'evil)
;;; evil.el ends here
