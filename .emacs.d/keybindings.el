;;; keybindings.el --- All custom keybindings -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: (evil)
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

;; Convenience file for storing all keybindings from different
;; conviguration modules in a single location.

;;; Code:

;; demote errors when exiting emacs
(global-set-key (kbd "C-x C-c")
		(lambda () (interactive)
		  (ignore-errors (save-buffers-kill-terminal)))
		)

;; toggle syntax type for a character between symbol and word
(bind-key* "C-c s" 'toggle-syntax-entry)

;;; layout keybindings

(with-eval-after-load "layout"

  ;;; manipulation of workspace tabs in tab-bar mode

  ;; customize tab bar keybindings
  (bind-key* "M-<tab>" 'tab-bar-switch-to-next-tab)
  (bind-key* "M-S-<iso-lefttab>" 'tab-bar-switch-to-prev-tab)
  (bind-key* "M-S-<tab>" 'tab-bar-switch-to-prev-tab)
  (bind-key* "M-w" 'tab-bar-switch-to-recent-tab)
  (bind-key* "M-j" 'tab-bar-select-tab)

  ;; shortkey for ace-window
  (bind-key* "M-o" 'ace-window)
  

  ;; ;;; buffer tab manipulation via awesome-tab
  
  ;; ;; previous tab, next tab, jump to tab
  ;; (global-set-key (kbd "C-x C-l") 'awesome-tab-forward-tab)
  ;; (global-set-key (kbd "C-x C-h") 'awesome-tab-backward-tab)
  ;; (global-set-key (kbd "C-x j") 'awesome-tab-ace-jump)

  ;; ;; next/previous tab group
  ;; (global-set-key (kbd "C-x C-j") 'awesome-tab-forward-group)
  ;; (global-set-key (kbd "C-x C-k") 'awesome-tab-backward-group)

  ;; ;; open minibuffer prompt for group name
  ;; (global-set-key (kbd "C-x C-n") 'awesome-tab-counsel-switch-group)

  ;;; buffer tab manipulation via centaur-tabs
  
  ;; previous tab, next tab, jump to tab
  (bind-key* "C-x C-l" 'centaur-tabs-forward)
  (bind-key* "C-x C-h" 'centaur-tabs-backward)
  ;; (global-set-key (kbd "C-x j") 'awesome-tab-ace-jump)

  ;; next/previous tab group
  (bind-key* "C-x C-j" 'centaur-tabs-forward-group)
  (bind-key* "C-x C-k" 'centaur-tabs-backward-group)

  ;; open minibuffer prompt for group name
  (bind-key* "C-x C-n" 'centaur-tabs-counsel-switch-group)

  ;; go to previous active buffer with C-x C-p
  (bind-key* "C-x C-p" 'previous-buffer)

  ;; list buffers using ibuffer (prettier output)
  (bind-key* "C-x C-b" 'ibuffer-list-buffers)


  ;;; visual window and buffer manipulation

  ;; toggle single-window mode with "C-w o" in normal state
  (evil-define-key 'normal 'global
    (kbd "C-w o") 'view-single-window-toggle)
  ;; load different buffer in same window with "C-w w"
  (evil-define-key 'normal 'global
    (kbd "C-w w") 'ivy-switch-buffer)
  ;; split and prompt for new buffer
  (evil-define-key 'normal 'global
    (kbd "C-w j") 'view-new-buffer-below)
  (evil-define-key 'normal 'global
    (kbd "C-w l") 'view-new-buffer-right)
  (evil-define-key 'normal 'global
    (kbd "C-w k") 'view-new-buffer-above)
  (evil-define-key 'normal 'global
    (kbd "C-w h") 'view-new-buffer-left)

  ;; keybindings for resizing windows (M-<arrows>)
  (bind-key* "M-<left>" 'shrink-window-horizontally)
  (bind-key* "M-<right>" 'enlarge-window-horizontally)
  (bind-key* "M-<down>" 'shrink-window)
  (bind-key* "M-<up>" 'enlarge-window)

  ;; kill current buffer with C-q
  (bind-key* "C-q" 'kill-this-buffer)

  ;;; operations in/on the treemacs buffer
  
  ;; toggle treemacs window with "C-x C-a"
  (evil-define-key '(normal insert) 'global (kbd "C-x C-a") 'treemacs)
  (define-key evil-treemacs-state-map (kbd "C-x C-a") #'treemacs-quit)

  ;; fold/unfold directories and tag lists with "za"
  (define-key evil-treemacs-state-map (kbd "za") #'treemacs-toggle-node)

  ;; move out of the treemacs window with "C-l"
  (define-key evil-treemacs-state-map (kbd "C-l") #'other-window)


  ;; create a file/directory with "C-x C-f,d"
  (define-key evil-treemacs-state-map (kbd "C-x C-f") #'treemacs-create-file)
  (define-key evil-treemacs-state-map (kbd "C-x C-d") #'treemacs-create-dir)

  ;; open new files in hsplit with "C-return"
  (define-key evil-treemacs-state-map (kbd "<C-return>")
    #'treemacs-visit-node-ace-vertical-split)


  )

;;; "evil" keybindings

(with-eval-after-load "evil" (with-no-warnings


  ;;; Global settings

  ;; close current buffer with Q
  (evil-define-key '(normal visual) 'global (kbd "Q") 'evil-quit)

  ;; add vim leader keybindings support
  (evil-leader/set-key

    ;; help
    "h" 'describe-key ; envoke emacs' C-h k with <leader>h
    "f" 'describe-function ; envoke emacs' C-h f with <leader>f
    "v" 'describe-variable ; envoke emacs' C-h v with <leader>v

    ;; error navigation
    "e" 'flycheck-next-error ; goto next error with <leader>e
    "p" 'flycheck-previous-error ; goto previous error with <leader>e

    ;; search
    "s" 'swiper-thing-at-point ; search thing at point with <leader>s
    "C-s" 'swiper-all-thing-at-point ; search thing at point in all
					; buffers with <leader>C-s

    ;; avy jumps
    "j" 'avy-goto-line-below
    "k" 'avy-goto-line-above
    "w" 'avy-goto-char-timer

    )

  ;;; Keybindings for normal mode

  ;; map j to gj, k to gk
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)

  ;; scroll up with C-u (automatic setting breaks if evil-leader is added)
  (evil-define-key 'normal 'global (kbd "C-u")
    (lambda () (interactive) (evil-scroll-up 0)))
  (evil-define-key 'normal 'global (kbd "C-d")
    (lambda () (interactive) (evil-scroll-down 0)))

  ;; for moving around use <C-h,j,k,l>
  (evil-define-key '(normal motion) 'global
    (kbd "C-h") 'evil-window-left)
  (evil-define-key '(normal motion) 'global
    (kbd "C-j") 'evil-window-down)
  (evil-define-key '(normal motion) 'global
    (kbd "C-k") 'evil-window-up)
  (evil-define-key '(normal motion) 'global
    (kbd "C-l") 'evil-window-right)

  ;; move panes with C-w C-h,j,k,l
  (evil-define-key '(normal motion) 'global
    (kbd "C-w C-h") 'evil-window-move-far-left)
  (evil-define-key '(normal motion) 'global
    (kbd "C-w C-j") 'evil-window-move-very-bottom)
  (evil-define-key '(normal motion) 'global
    (kbd "C-w C-k") 'evil-window-move-very-top)
  (evil-define-key '(normal motion) 'global
    (kbd "C-w C-l") 'evil-window-move-far-right)

  ;; use swiper as default search backend
  (evil-define-key '(normal motion) 'global
    (kbd "/") 'swiper)
  (evil-define-key '(normal motion) 'global
    (kbd "?") 'swiper-backward)

  ;; search all buffers using swiper
  (global-unset-key (kbd "C-s"))
  (evil-define-key '(normal motion) 'global
    (kbd "C-s") 'swiper-all)

  ;; fold everything using zA
  (evil-define-key 'normal 'global (kbd "zA") 'hs-hide-all)

  ;; fold level using zz
  (evil-define-key 'normal 'global (kbd "zz") 'hs-hide-level)


  ;; function and mapping to add line below and return to normal mode
  (defun evil-newline-after ()
    (interactive)
    (evil-open-below 1)
    (evil-normal-state))
  (evil-define-key 'normal 'global
    (kbd "RET") 'evil-newline-after)

  ;; indent and unindent line with ">", "<" and preserve cursor
  (evil-define-key 'normal 'global (kbd ">")
    (lambda () (interactive) (save-excursion (evil-shift-right-line 1))))
  (evil-define-key 'normal 'global (kbd "<")
    (lambda () (interactive) (save-excursion (evil-shift-left-line 1))))


  ;; Keybindings for visual mode

  ;; keep visual selection after indenting
  (evil-define-operator visual-indent-right (BEG END)
    (evil-shift-right BEG END)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-operator visual-indent-left (BEG END)
    (evil-shift-left BEG END)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-key 'visual 'global
    (kbd ">") 'visual-indent-right)
  (evil-define-key 'visual 'global
    (kbd "<") 'visual-indent-left)


  ;; Keybindings for insert mode
	
  ;; expand tabs into fixed number of spaces
  (evil-define-key 'insert 'global
    (kbd "TAB") 'tab-to-tab-stop)

  ;; use C-h,l to jump out of parentheses
  (evil-define-key 'insert 'global
    (kbd "C-l") 'sp-forward-sexp)
  (evil-define-key 'insert 'global
    (kbd "C-h") 'sp-backward-sexp)

  ;; jump words with "C-f,b"
  (evil-define-key 'insert 'global
    (kbd "C-f") 'forward-word)
  (evil-define-key 'insert 'global
    (kbd "C-b") 'backward-word)

  ;; envoke help command with "M-h,f,v"
  (evil-define-key 'insert 'global
    (kbd "M-h") 'describe-key)
  (evil-define-key 'insert 'global
    (kbd "M-f") 'describe-function)
  (evil-define-key 'insert 'global
    (kbd "M-v") 'describe-variable)

  ;; continue with comments or indentation on newline with C-RET
  (evil-define-key 'insert 'global
    (kbd "C-<return>") 'comment-indent-new-line)

  ))


;;; "terminal" keybindings

(with-eval-after-load "terminal"

  ;; explicitly map escape key to raw meta key because another mapping
  ;; in ivy messes with this default behaviour
  (evil-define-key 'emacs 'term-raw-map
    (kbd "<escape>") 'term-send-raw-meta)
  ;; (bind-key "<escape>" 'term-send-raw-meta 'term-raw-map)

  ;; bind escape key in vterm-mode as well
  (evil-define-key 'emacs 'vterm-mode-map
    (kbd "<escape>") 'vterm-send-escape)

  ;; prefix and command sequence bindings for opening terminal shells
  (global-unset-key (kbd "M-s"))

  ;;; DEPRECATED IN FAVOUR OF VTERM:
  ;; ;; use "M-s t" to open EAF terminal
  ;; (with-eval-after-load "widgets"
  ;;   (with-eval-after-load "evil"
  ;;     (global-set-key (kbd "M-s t")
  ;; 		      '(lambda() (interactive)
  ;; 			(evil-window-split) (eaf-open-terminal)))
  ;;   ;; map escape key to itself in EAF terminal
  ;;   (defun eaf-send-raw-escape ()
  ;;     "Directly send <escape> key to EAF Python side."
  ;;     (interactive)
  ;;     (eaf-call-async "send_key_sequence" eaf--buffer-id "M-d"))
  ;;   (eaf-bind-key eaf-send-raw-escape "<escape>"
  ;; 		  eaf-terminal-keybinding) ;; unbind, see more in the Wiki
  ;;     ))

  ; keybinding to open term buffer in emacs state in new window
  (global-set-key
   (kbd "M-s s")
   '(lambda (p) (interactive "P")
      (get-or-create-terminal p nil nil t)))

  ;; keybinding to send text region as commands to a terminal buffer
  (global-set-key
   (kbd "M-s M-s")
   '(lambda (p) (interactive "P")
      (get-or-create-terminal
       p nil nil nil 'evil-send-region-to-terminal)
      )
   )

  ;; keybinding for pasting text from default register
  (evil-define-key 'normal vterm-mode-map
    (kbd "p") '(lambda () (interactive)
		   (forward-char) (vterm-yank)))

  ;; keybinding for pasting text from clipboard
  (evil-define-key '(insert emacs) vterm-mode-map
    (kbd "M-v") 'vterm-yank-primary)
  (evil-define-key 'normal vterm-mode-map
    (kbd "M-v") '(lambda () (interactive)
		   (forward-char) (vterm-yank-primary)))

  ;; keybinding for pasting text from kill ring
  (evil-define-key '(insert emacs) vterm-mode-map
    (kbd "M-y") 'vterm-yank-pop)
  (evil-define-key 'normal vterm-mode-map
    (kbd "M-y") '(lambda () (interactive)
		   (forward-char) (vterm-yank-pop)))

  )



;;; "ide-base" keybindings

(with-eval-after-load "ide"

  ;; escape ivy minibuffer with escape key
  (define-key ivy-minibuffer-map (kbd "<escape>")
    'minibuffer-keyboard-quit)

  ;; use "C-a" to show company popup in normal and insert states
  ;; (evil-define-key '(normal insert) 'global (kbd "C-a") 'toggle-company-idle-delay)
  (evil-define-key '(normal insert) 'global (kbd "C-a") 'company-complete)

  ;; use "C-y" to envoke company-yasnippet specifically
  (evil-define-key '(normal insert) 'global (kbd "C-y") 'company-yasnippet)

  ;; map tab key to yasnippet completion
  (evil-define-key 'insert 'yas-minor-mode-map (kbd "<tab>") yas-maybe-expand)
  (evil-define-key 'insert 'yas-minor-mode-map (kbd "TAB") yas-maybe-expand)

  ;; browse kill-ring with counsel
  (evil-define-key 'normal 'global (kbd "M-y") 'my/counsel-paste-pop)

  ;; also, when in visual mode, delete highlighted text first
  (evil-define-key 'visual 'global (kbd "M-y") 'my/counsel-yank-pop-selection)

  ;; format braces
  (evil-define-key 'normal 'global (kbd "C-e") 'ide/format-parens)

  ;; leader key mappings
  (evil-leader/set-key

    ;; show documentation in LSP-UI
    "d" 'lsp-ui-doc-show

    ;; switch to LSP UI doc frame
    "<tab>" 'lsp-ui-doc-focus-frame

    ;; close LSP UI doc frame
    "q" '(lambda () (interactive)
	   (lsp-ui-doc-unfocus-frame) (lsp-ui-doc-hide))

    )

  )

;;; "snippets" keybinding

(with-eval-after-load "snippets"

  ;; navigate fields in a snippet with "C-h,l"
  (define-key yas-keymap (kbd "<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<backtab>") nil)
  (define-key yas-keymap (kbd "S-TAB") nil)
  (define-key yas-keymap (kbd "C-l") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-h") 'yas-prev-field)

  )

;;; "k8s" keybindings

(with-eval-after-load "k8s"

  ;; prefix for Kubernetes commands
  (global-unset-key (kbd "M-k"))

  ;; open kubetneres buffer
  (global-set-key (kbd "M-k k")
		  '(lambda()
		    (interactive)
		    (tab-new)
		    (kubernetes-overview)
		    (tab-close)
		    (other-window 1)
		    (evil-split-buffer "*kubernetes overview*")
		    ))

  ;; change namespace in kubernetes buffer
  (global-set-key (kbd "M-k n") 'kubernetes-set-namespace)

  ;; exec into pod from kubernetes buffer
  (global-set-key (kbd "M-k e") 'kubernetes-exec-into)

  )

;; ;;; "openvpn" keybindings

;; (with-eval-after-load "openvpn"

;;   ;; use "C-s l" to open ovpn dashboard in new buffer
;;   (global-set-key (kbd "M-s l")
;; 		  '(lambda() (interactive) (other-window 1)
;; 		     (evil-split-buffer "*scratch*") (ovpn)))
;;   )

;;; "org" keybindings

(with-eval-after-load "org"

  ;; preserve tab-bar movement commads; move headings around differently
  (define-key outline-mode-map (kbd "M-j") nil)
  (define-key outline-mode-map (kbd "M-k") nil)
  (with-eval-after-load "evil"
    (evil-define-key 'normal outline-mode-map (kbd "M-j") nil)
    (evil-define-key 'normal outline-mode-map (kbd "M-k") nil)
    (evil-define-key 'normal org-mode-map (kbd "C-M-j") \
      'outline-move-subtree-down)
    (evil-define-key 'normal org-mode-map (kbd "C-M-k") \
      'outline-move-subtree-up)
    )

  ;; preserve window movement commands; jump around headings differently
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-k") nil)
  (with-eval-after-load "evil"
    (evil-define-key 'normal org-mode-map (kbd "C-j") nil)
    (evil-define-key 'normal org-mode-map (kbd "C-k") nil)
    (evil-define-key 'normal org-mode-map (kbd "C-n")
      'org-forward-heading-same-level)
    (evil-define-key 'normal org-mode-map (kbd "C-p")
      'org-backward-heading-same-level)
    )

  ;; toggle checkboxes more easily
  (define-key org-mode-map (kbd "C-c x") 'org-toggle-checkbox)

  ;; add property with C-c C-p
  (define-key org-mode-map (kbd "C-c C-p") 'org-set-property)

  ;; trigger for verb command map
  (define-key org-mode-map (kbd "C-c C-r") 'verb-command-map)

  ;; toggle URL visibility with "C-c w w"
  (define-key org-mode-map (kbd "C-c w w") 'org-toggle-link-display)

  ;; open link at point with EAF using "C-c w o"
  ;; NOTE: this does not override the default Org keybinding "C-c C-o"
  (define-key org-mode-map (kbd "C-c w o")
    (lambda () (interactive)
      (evil-window-vsplit) (eaf-open-url-at-point)))

  ;; add useful keybindings to emails as well
  (with-eval-after-load "email"

    ;; open messages with org keybinding in mu4e-view-mode
    (define-key mu4e-view-mode-map (kbd "C-c C-o") 'org-open-at-point)
    (define-key mu4e-view-mode-map (kbd "C-c w w")
      (lambda () (interactive)
	(evil-window-vsplit) (eaf-open-mail-as-html)))
    (define-key mu4e-view-mode-map (kbd "C-c w o")
      (lambda () (interactive)
	(evil-window-vsplit) (eaf-open-url-at-point)))

    )

  )

;;; "python" keybindings

(with-eval-after-load "python"

  ;; keybinding to open python shell buffer in emacs state in new window
  (define-key python-mode-map
    (kbd "M-s p") 'my-python-run-shell-in-terminal)

  ;; keybinding to send visual selection or code section to shell
  (define-key python-mode-map
    (kbd "M-s M-p") 'my-python-execute-code-block)
  (define-key python-mode-map
    (kbd "M-s M-<return>") '(lambda (p) (interactive "P")
			      (my-python-execute-code-block p)
			      (python-forward-fold-or-section)))

  ;; navigate code statements
  (evil-define-key 'normal python-mode-map
    (kbd "C-n") 'python-nav-forward-statement)
  (evil-define-key 'normal python-mode-map
    (kbd "C-p") 'python-nav-backward-statement)

  ;; navigate code blocks
  (evil-define-key 'normal python-mode-map
    (kbd "C-S-n") 'python-nav-forward-block)
  (evil-define-key 'normal python-mode-map
    (kbd "C-S-p") 'python-nav-backward-block)

  ;; navigate function, method and class definitions
  (evil-define-key 'normal python-mode-map
    (kbd "C-M-n") 'python-nav-forward-defun)
  ;; NOTE: using lambda function so that the cursor jumps to the end of
  ;; the previous function/class declaration.  Default behaviour of the
  ;; python nav command puts the cursor before a declaration, starting
  ;; with the current function.
  (evil-define-key 'normal python-mode-map
    (kbd "C-M-p") '(lambda () (interactive)
		   (python-nav-backward-defun)
		   (python-nav-backward-defun)
		   (python-nav-forward-defun)))

  ;; jump code sections with C-M-j / C-M-k
  (evil-define-key 'normal python-mode-map
    (kbd "C-M-j") '(lambda () (interactive)
		     (python-forward-fold-or-section)
		     (evil-next-visual-line)))
  (evil-define-key 'normal python-mode-map
    (kbd "C-M-k") '(lambda () (interactive)
		     (evil-previous-visual-line)
		     (python-backward-fold-or-section)
		     (evil-next-visual-line)))

  ;;; NOTE: <leader>[j,k] for python deprecated in favour of avy-jump
  ;; ;; leader key mappings
  ;; (evil-leader/set-key

  ;;   ;; block-wise code navigation: forward
  ;;   "j" (lambda () (interactive)
  ;; 	  (python-forward-fold-or-section)
  ;; 	  (evil-next-visual-line))

  ;;   ;; block-wise code navigation: backwards
  ;;   "k" (lambda () (interactive)
  ;; 	  (python-backward-fold-or-section)
  ;; 	  (python-backward-fold-or-section)
  ;; 	  (evil-next-visual-line))

  ;;   ;; ;; block-wise code execution
  ;;   ;; "r" (lambda () (interactive)
  ;;   ;; 	  )
  ;;   )

    )

(provide 'keybindings)
;;; keybindings.el ends here
