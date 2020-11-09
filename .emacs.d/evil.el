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

  ;; close current buffer with Q
  (evil-define-key '(normal visual) 'global (kbd "Q") 'evil-quit)

  ;; open terminal shells in normal state
  (evil-set-initial-state 'term-mode 'normal)


  ;;; Keybindings for normal mode

  ;; map j to gj, k to gk
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)

  ;; scroll up with C-u (automatic setting breaks if evil-leader is added)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)

  ;; for moving around use <C-h,j,k,l>
  (evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)

  ;; move panes with C-w C-h,j,k,l
  (evil-define-key 'normal 'global (kbd "C-w C-h") 'evil-window-move-far-left)
  (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-move-very-bottom)
  (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-move-very-top)
  (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-move-far-right)


  ;; function and mapping to add line below and return to normal mode
  (defun evil-newline-after ()
    (interactive)
    (evil-open-below 1)
    (evil-normal-state))
  (evil-define-key 'normal 'global (kbd "RET") 'evil-newline-after)

  ;; indent and unindent line with ">", "<" and preserve cursor
  (evil-define-key 'normal 'global (kbd ">")
    (lambda () (interactive) (save-excursion (evil-shift-right-line 1))))
  (evil-define-key 'normal 'global (kbd "<")
    (lambda () (interactive) (save-excursion (evil-shift-left-line 1))))


  ;; Keybindings for visual mode

  ;; keep visual selection after indenting
  (evil-define-operator visual-indent-right (BEG END)
    (evil-shift-right BEG END) (evil-normal-state) (evil-visual-restore))
  (evil-define-operator visual-indent-left (BEG END)
    (evil-shift-left BEG END) (evil-normal-state) (evil-visual-restore))
  (evil-define-key 'visual 'global (kbd ">") 'visual-indent-right)
  (evil-define-key 'visual 'global (kbd "<") 'visual-indent-left)


  ;; Keybindings for insert mode
	
  ;; expand tabs into fixed number of spaces
  (evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)

  ;; use C-h,l to jump out of parentheses
  (evil-define-key 'insert 'global (kbd "C-l") 'sp-forward-sexp)
  (evil-define-key 'insert 'global (kbd "C-h") 'sp-backward-sexp)

  ;; jump words with "C-f,b"
  (evil-define-key 'insert 'global (kbd "C-f") 'forward-word)
  (evil-define-key 'insert 'global (kbd "C-b") 'backward-word)

  ;; envoke help command with "M-h,f,v"
  (evil-define-key 'insert 'global (kbd "M-h") 'describe-key)
  (evil-define-key 'insert 'global (kbd "M-f") 'describe-function)
  (evil-define-key 'insert 'global (kbd "M-v") 'describe-variable)

  ;; continue with comments or indentation on newline with C-RET
  (evil-define-key 'insert 'global (kbd "C-<return>") 'comment-indent-new-line)

  ))

;; add vim leader key support
(use-package evil-leader
  :config (with-no-warnings
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "h" 'describe-key ; envoke emacs' C-h k with <leader>h
   "f" 'describe-function ; envoke emacs' C-h f with <leader>f
   "v" 'describe-variable ; envoke emacs' C-h v with <leader>v
   "e" 'flycheck-next-error ; goto next error with <leader>e
   "p" 'flycheck-previous-error ; goto previous error with <leader>e
   )))

;; surround.vim
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; vim-commentary
(use-package evil-commentary
  :config
  (evil-commentary-mode))


(provide 'evil)
;;; evil.el ends here
