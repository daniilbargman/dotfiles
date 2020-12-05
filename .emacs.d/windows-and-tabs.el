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


;;; Define variables for this customization

;; group under "bind-terminal-shell"
(defgroup windows-and-tabs nil
  "Variables for setting behaviour of windows and tabs, including buffer
groups and filters"
  :group 'External
  :prefix 'windows-and-tabs
  :version '0.1.0)

;: alist mapping buffer name prefixes to tab groups
(defcustom windows-and-tabs-buffer-groups-by-name-regex nil
  "Alist mapping buffer name regexp expressions to buffer group names."
  :group 'windows-and-tabs
  :type '(alist :key-type regexp :value-type string)
  :safe (lambda (x) t))

;; alist mapping buffer major modes to tab groups
(defcustom windows-and-tabs-buffer-groups-by-major-mode nil
  "Alist mapping buffer major modes to buffer group names."
  :group 'windows-and-tabs
  :type '(alist :key-type sexp :value-type string)
  :safe (lambda (x) t))

;; list of buffer names to filter out from awesome-tab
(defcustom windows-and-tabs-buffer-filter-regexp-list nil
  "Alist mapping buffer major modes to buffer group names."
  :group 'windows-and-tabs
  :type '(repeat regexp)
  :safe (lambda (x) t))


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
(global-set-key (kbd "M-j") 'tab-bar-select-tab)


;;; Support creating / maximizing / destroying windows
(window-list)

;; keybindings for resizing windows (M-<arrows>)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

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
(defun default-awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from
`eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Systema\".
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
     "*System*")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Emacs-config")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (awesome-tab-get-group-name (current-buffer))))))


;; custom function for defining buffer groups in awesome tab
(defun awesome-tab-buffer-groups ()
  "Define awesome tab buffer groups based on custom variables.

Variables are `windows-and-tabs-buffer-groups-by-name-regex' and
`windows-and-tabs-buffer-groups-by-major-mode'.  Both are alists mapping
buffer attributes (name regex / major mode) to buffer group names.

The final result is the name derived from the name regex match, followed
by the name derived from the major mode match, separated by a dash.
Only the first valid match from each alist is used.  If no matches have
been found, \"unmatched\" is used for the name regex component and
\"other\" is used for the major mode component.

If one of the alists is nil, only the other alist is used for grouping.

If both alists are nil, `default-awesome-tab-buffer-groups' is applied."

  ;; pre-allocate buffer name output
  (defvar buffer-name-list)
  (setq buffer-name-list (list))

  ;; if name regex mapping is defined, apply it
  (unless (null windows-and-tabs-buffer-groups-by-name-regex)
    (add-to-list 'buffer-name-list
	    (cl-loop
	     for (regex-value . group-name)
		in windows-and-tabs-buffer-groups-by-name-regex
	      if (string-match regex-value (buffer-name))
		  return group-name
	      finally
		;; return "unmatched")
		(awesome-tab-get-group-name (current-buffer)))
	    t))

  ;; if major mode mapping is defined, apply it
  (unless (null windows-and-tabs-buffer-groups-by-major-mode)
    (add-to-list 'buffer-name-list
	    (cl-loop
	     for (major-mode-value . group-name)
		in windows-and-tabs-buffer-groups-by-major-mode
	      if (derived-mode-p major-mode-value)
		  return group-name
	      finally
		;; return "other")
		(awesome-tab-get-group-name (current-buffer)))
	    t))

  ;; return final value or the output of the fallback function
  (if (null buffer-name-list) (default-awesome-tab-buffer-groups)
	  (list (mapconcat 'identity buffer-name-list "-"))))


;; default buffer filter function
(defun default-awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (and (string-prefix-p "magit" name)
               (not (file-name-extension name)))
     )))

;; custom buffer filter function with fallback to default
(defun awesome-tab-hide-tab (x)
  "Hide buffer X from awesome-tab buffer list based on custom variable.

The variable that determines the filter's behaviour is
`windows-and-tabs-buffer-filter-regexp-list'.  It is a list of regexp
values, and any positive match will eliminate the buffer from awesome
tab's grouping collage."
  (if (null windows-and-tabs-buffer-filter-regexp-list)
      (default-awesome-tab-hide-tab x)
      (let ((name (format "%s" x)))
	(cl-loop for buffer-regexp-value
		in windows-and-tabs-buffer-filter-regexp-list
		  if (string-match buffer-regexp-value name)
		      return t
		  finally
		      return nil))))

(let ((name (format "%s" (get-buffer "*ovpn-mode*"))))
  (cl-loop for buffer-regexp-value
	  in windows-and-tabs-buffer-filter-regexp-list
	    if (string-match buffer-regexp-value name)
		return t
	    finally
		return buffer-regexp-value))

;; bind Cache-x C-h for previous tab, C-x C-l for next tab, C-x C-u to jump
(global-set-key (kbd "C-x C-l") 'awesome-tab-forward-tab)
(global-set-key (kbd "C-x C-h") 'awesome-tab-backward-tab)
(global-set-key (kbd "C-x j") 'awesome-tab-ace-jump)

;; bind "C-x C-j,k" to go to next/previous group
(global-set-key (kbd "C-x C-j") 'awesome-tab-forward-group)
(global-set-key (kbd "C-x C-k") 'awesome-tab-backward-group)

;; bind "C-x C-n" to open minibuffer prompt for group name
(global-set-key (kbd "C-x C-n") 'awesome-tab-counsel-switch-group)


;;; Manage projects with treemacs

(use-package treemacs
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images		 t
          treemacs-no-delete-other-windows       nil  ; delete treemacs with other windows
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    ;; actions to perform with the tab key (open files in new tab)
    (setq treemacs-TAB-actions-config
	  '((root-node-open . treemacs-toggle-node)
	   (root-node-closed . treemacs-toggle-node)
	   (dir-node-open . treemacs-toggle-node)
	   (dir-node-closed . treemacs-toggle-node)
	   (file-node-open . treemacs-visit-node-no-split)
	   (file-node-closed . treemacs-visit-node-no-split)
	   (tag-node-open . treemacs-visit-node-no-split)
	   (tag-node-closed . treemacs-visit-node-no-split)
	   (tag-node . treemacs-visit-node-no-split)))

    ;; actions to perform with the return key (open files in vsplit)
    (setq treemacs-RET-actions-config
	  '((root-node-open . treemacs-toggle-node)
	   (root-node-closed . treemacs-toggle-node)
	   (dir-node-open . treemacs-toggle-node)
	   (dir-node-closed . treemacs-toggle-node)
	   (file-node-open . treemacs-visit-node-horizontal-split)
	   (file-node-closed . treemacs-visit-node-horizontal-split)
	   (tag-node-open . treemacs-visit-node-horizontal-split)
	   (tag-node-closed . treemacs-visit-node-horizontal-split)
	   (tag-node . treemacs-visit-node-horizontal-split)))

    ;; enable modes
    (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

  ))

;; evil mode support
(use-package treemacs-evil
  :after treemacs evil
  :config

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
    #'treemacs-visit-node-vertical-split)

  )

(provide 'windows-and-tabs)
;;; windows-and-tabs.el ends here