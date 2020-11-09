;;; windows-and-tabs.el --- windows and tabs -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: (use-package evil-mode ivy emacs27+)
;; Homepage: homepage
;; Keywords: convenience views tabs


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

;; Vim/Tmux-style functionality for managing views within tabs:
;;  - create / destroy / maximize window splits in evil normal state
;;; - use view tabs in frames, buffer tabs in windows

;;; Code:


;;; Support for preserving and toggling views (tab-bar-mode)
(tab-bar-mode 1)
(tab-bar-history-mode 1)

;; customize tab bar design
(setq tab-bar-position t)
(setq tab-bar-back-button "")
(setq tab-bar-forward-button "")
(setq tab-bar-close-button-show nil)
(setq tab-bar-button-margin 0)
(setq tab-bar-button-relief 0)
(setq tab-bar-border 2)
;; (setq tab-bar-separator "|")
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-tab-hints t)
(custom-set-faces
 '(tab-bar
   ((t (:background "#393939"
	:height 1.2
	))))
 '(tab-bar-tab
   ((t (:background "#393939"
	:foreground "#cc99cc"
	:box nil ; '(:line-width 1 :style nil)
	:inverse-video: nil
	:height 0.9
	))))
 '(tab-bar-tab-inactive
   ((t (:background "#999999"
	:foreground "#393939"
	:box nil ; '(:line-width 1 :style nil)
	:inverse-video: nil
	:height 0.9
	))))
 '(tab-line
   ((t (:background "#191919"
	))))
 )

;; customize tab bar keybindings
(global-set-key (kbd "M-<tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-S-<iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "M-j") 'tab-bar-select-tab-by-name)


;;; Support creating / maximizing / destroying windows

;; function to toggle single-window view
(defun view-single-window-toggle ()
   "Toggle between current layout and single-window view.

Uses winner-mode-undo."
   (interactive)

   ;; if we have multiple windows, save view as "toggle-view-temp"
   ;; and only keep the active window
   (cond
     ((> (length (window-list)) 1)
      (delete-other-windows))
   ;; if number of windows is 1, switch to "toggle-view-temp"
     ((= (length (window-list)) 1)
      (tab-bar-history-back))
   ))

;; create new window below current and open switch-buffer prompt
(defun view-new-buffer-below ()
  "Split current window horizontally and open a buffer prompt.

Uses evil commands."
  (interactive)
  ;; choose new buffer from menu
  (ivy-switch-buffer)
  ;; go back to current buffer in this window
  (previous-buffer)
  ;; make empty split below current window and switch to new buffer there
  (evil-window-split)
  (next-buffer))

;; create new window right of current and open switch-buffer prompt
(defun view-new-buffer-right ()
  "Split current window horizontally and open a buffer prompt.

Uses evil commands."
  (interactive)
  ;; choose new buffer from menu
  (ivy-switch-buffer)
  ;; go back to current buffer in this window
  (previous-buffer)
  ;; make empty split below current window and switch to new buffer there
  (evil-window-vsplit)
  (next-buffer))

;; toggle single-window mode with "C-w o" in normal state
(evil-define-key 'normal 'global (kbd "C-w o") 'view-single-window-toggle)
;; load different buffer in same window with "C-w w"
(evil-define-key 'normal 'global (kbd "C-w w") 'ivy-switch-buffer)
;; split and prompt for new buffer
(evil-define-key 'normal 'global (kbd "C-w j") 'view-new-buffer-below)
(evil-define-key 'normal 'global (kbd "C-w l") 'view-new-buffer-right)

;; kill current buffer with C-q
(global-set-key (kbd "C-q") 'kill-this-buffer)


;;; support for tabs and tab groups via awesome-tab

;; load package (from github as melpa doesn't have it yet)
(use-package awesome-tab
  :quelpa
  (awesome-tab :fetcher github :repo "manateelazycat/awesome-tab")
  :config

  ;; ;; cycle through groups not tabs
  ;; (setq awesome-tab-cycle-scope "groups")

  ;; make tab bar smaller
  (setq awesome-tab-height 110)  ; default is 150

  ;; display tab globally
  (setq awesome-tab-display-line 'tab-line)

  ;; enable mode
  (awesome-tab-mode t))

;; default function for defining buffer groups
(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from
`eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

;; bind C-x C-h for previous tab, C-x C-l for next tab, C-x C-u to jump
(global-set-key (kbd "C-x C-l") 'awesome-tab-forward-tab)
(global-set-key (kbd "C-x C-h") 'awesome-tab-backward-tab)
(global-set-key (kbd "C-x C-u") 'awesome-tab-ace-jump)

;; bind "C-x C-j,k" to go to next/previous group
(global-set-key (kbd "C-x C-j") 'awesome-tab-forward-group)
(global-set-key (kbd "C-x C-k") 'awesome-tab-backward-group)

;; bind "C-x C-n" to open minibuffer prompt for group name
(global-set-key (kbd "C-x C-n") 'awesome-tab-counsel-switch-group)

;; ;;; alternative tab manager: centaur tabs

;; ;; load package
;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-headline-match)
;;   :custom
;;   (centaur-tabs-style "bar")  ; change the way tabs look
;;   (centaur-tabs-set-close-button nil)  ; hide "close" button
;;   (centaur-tabs-set-modified-marker t)  ; show if buffer was modified
;;   (centaur-tabs-modified-marker "*")
;;   (centaur-tabs-cycle-scope 'tabs)  ; do not cycle past single group
;;   :bind
;;   ("C-x C-l" . centaur-tabs-forward)
;;   ("C-x C-h" . centaur-tabs-backward))

;; ;; function for defining tab groups
;; (defun centaur-tabs-buffer-groups ()
;;   "`centaur-tabs-buffer-groups' control buffers' group rules.

;; Group centaur-tabs with mode if buffer is derived from
;; `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;; All buffer name start with * will group to \"Emacs\".
;; Other buffer group by `centaur-tabs-get-group-name' with project name."
;;   (list
;;     (cond
;;       ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	  (memq major-mode '(magit-process-mode
;; 			      magit-status-mode
;; 			      magit-diff-mode
;; 			      magit-log-mode
;; 			      magit-file-mode
;; 			      magit-blob-mode
;; 			      magit-blame-mode
;; 			      )))
;;       "Emacs")
;;       ((derived-mode-p 'prog-mode)
;;       "Editing")
;;       ((derived-mode-p 'dired-mode)
;;       "Dired")
;;       ((memq major-mode '(helpful-mode
;; 			  help-mode))
;;       "Help")
;;       ((memq major-mode '(org-mode
;; 			  org-agenda-clockreport-mode
;; 			  org-src-mode
;; 			  org-agenda-mode
;; 			  org-beamer-mode
;; 			  org-indent-mode
;; 			  org-bullets-mode
;; 			  org-cdlatex-mode
;; 			  org-agenda-log-mode
;; 			  diary-mode))
;;       "OrgMode")
;;       (t
;;       (centaur-tabs-get-group-name (current-buffer))))))

;; ;; bind C-x C-h for previous tab, C-x C-l for next tab, C-x C-j to jump
;; (global-set-key (kbd "C-x C-l") 'centaur-tabs-forward)
;; (global-set-key (kbd "C-x C-h") 'centaur-tabs-backward)

;; ;; bind M-<tab> to go to next group, M-s-<tab> to go to previous group
;; (global-set-key (kbd "M-<tab>") 'centaur-tabs-forward-group)
;; (global-set-key (kbd "M-S-<iso-lefttab>") 'centaur-tabs-backward-group)

(provide 'windows-and-tabs)
;;; windows-and-tabs.el ends here
