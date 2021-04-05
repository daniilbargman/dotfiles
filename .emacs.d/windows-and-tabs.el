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
  "Variables for setting behaviour of windows and tabs."
  :group 'External
  :prefix 'windows-and-tabs
  :version '0.1.0)

;: alist mapping buffer name prefixes to tab groups
(defcustom windows-and-tabs-buffer-groups-by-name-regex nil
  "Alist mapping buffer name regexp expressions to buffer group names."
  :group 'windows-and-tabs
  :type '(alist :key-type regexp :value-type string)
  :safe (lambda (_) t))

;; alist mapping buffer major modes to tab groups
(defcustom windows-and-tabs-buffer-groups-by-major-mode nil
  "Alist mapping buffer major modes to buffer group names."
  :group 'windows-and-tabs
  :type '(alist :key-type sexp :value-type string)
  :safe (lambda (_) t))

;; alist mapping buffer file paths to tab groups
(defcustom windows-and-tabs-buffer-groups-by-project-path nil
  "Alist mapping project paths to buffer group names."
  :group 'windows-and-tabs
  :type '(alist :key-type string :value-type string)
  :safe (lambda (_) t))

;; list of buffer names to filter out from awesome-tab
(defcustom windows-and-tabs-buffer-filter-regexp-list nil
  "Alist mapping buffer major modes to buffer group names."
  :group 'windows-and-tabs
  :type '(repeat regexp)
  :safe (lambda (_) t))


;;; Support for preserving and toggling views (tab-bar-mode)
(tab-bar-mode 1)
(tab-bar-history-mode 1)


;; remove tab-bar header line from popup frames
(advice-add
 'display-buffer-in-child-frame :filter-return
 #'(lambda (window) (toggle-frame-tab-bar (window-frame window)) window))


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
   ((t (:background "#202020"
	:height 1.2
	))))
 '(tab-bar-tab
   ((t (:background "#202020"
	:foreground "#78B9C5" ; "#7ec98f"
	:box nil ; '(:line-width 1 :style nil)
	:inverse-video: nil
	:height 0.9
	))))
 '(tab-bar-tab-inactive
   ((t (:background "#202020"
	:foreground "#999999"
	:box nil ; '(:line-width 1 :style nil)
	:inverse-video: nil
	:height 0.9
	))))
 '(tab-line
   ((t (:background "#101010"
	))))
 )


;;; Support creating / maximizing / destroying windows
(window-list)

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


;;; support for tabs and tab groups via awesome-tab

;; load package (from github as melpa doesn't have it yet)
(use-package centaur-tabs
  :demand
  :custom

  ;; shape and size of the tabs
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 32)

  ;; support for plain icons
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)

  ;; underline active tab
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)

  ;; disable close button
  (centaur-tabs-set-close-button nil)

  ;; show dots next to modified tabs
  (centaur-tabs-set-modified-marker t)

  ;; adjust ordering of buffers based on usage
  (centaur-tabs-adjust-buffer-order 'left)
  
  :config

  ;; add support for icons
  (use-package all-the-icons
    :config (all-the-icons-install-fonts t))

  ;; enable mode
  (centaur-tabs-mode t)

  ;; match headlin style
  (centaur-tabs-headline-match)

  ;; match headlin style
  (centaur-tabs-enable-buffer-reordering)

  ;; default function for defining buffer groups
  (defun default-centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

  Group awesome-tab with mode if buffer is derived from
  `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
  All buffer name start with * will group to \"Systema\".
  Other buffer group by `centaur-tabs-get-group-name' with project name."
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
      (centaur-tabs-get-group-name (current-buffer))))))


  ;; custom function for defining buffer groups in awesome tab
  (defun centaur-tabs-buffer-groups ()
    "Define centaur tabs buffer groups based on custom variables.

  Variables are:

    `windows-and-tabs-buffer-groups-by-name-regex'
    `windows-and-tabs-buffer-groups-by-major-mode'
    `windows-and-tabs-buffer-groups-by-project-path'

  All three are alists mapping buffer attributes (name regex / major
  mode / file path) to buffer group names.

  The final result is a concated list of regex-based groups, major
  mode-based groups, and file path-based groups, in that order
  (i.e. only buffers not captured by the previous alist are forwarded
  to the next alist).

  If all alists are nil, `default-centaur-tabs-buffer-groups' is used."

    ;; if name regex mapping is defined, apply it
    (let ((buffer-group-name nil))
      (setq buffer-group-name
	    (cl-loop
	    for (regex-value . group-name)
	    in windows-and-tabs-buffer-groups-by-name-regex
	    if (string-match regex-value (buffer-name))
	    return group-name
		;; finally
		;; 	;; return "unmatched")
		;; 	(awesome-tab-get-group-name (current-buffer))
	    ))

      ;; if major mode mapping is defined, apply it
      (unless buffer-group-name
	(setq buffer-group-name
	      (cl-loop
	      for (major-mode-value . group-name)
	      in windows-and-tabs-buffer-groups-by-major-mode
	      if (derived-mode-p major-mode-value)
	      return group-name
	      )))

      ;; if project path mapping is defined, apply it
      (unless buffer-group-name
	(let ((bufferpath (buffer-file-name)))
	  (if bufferpath
	    (setq buffer-group-name
		  (cl-loop
		  for (project-path-value . group-name)
		  in windows-and-tabs-buffer-groups-by-project-path
		  if (string-prefix-p project-path-value bufferpath)
		  return group-name
		  )))))

      ;; if at least one classification is present but tab hasn't been
      ;; classified, return default group
      (if
	  (and
	    (null buffer-group-name)
	    (not (and (null windows-and-tabs-buffer-groups-by-major-mode)
		      (null windows-and-tabs-buffer-groups-by-name-regex)
		      (null windows-and-tabs-buffer-groups-by-project-path)
		      )))
	  (setq buffer-group-name
		(centaur-tabs-get-group-name (current-buffer))))

      ;; return final value or the output of the fallback function
      (if (null buffer-group-name) (default-centaur-tabs-buffer-groups)
	      (list buffer-group-name))))


  ;; default buffer filter function
  (defun default-centaur-tabs-hide-tab (x)
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
  (defun centaur-tabs-hide-tab (x)
    "Hide buffer X from centaur-tabs buffer list based on custom variable.

  The variable that determines the filter's behaviour is
  `windows-and-tabs-buffer-filter-regexp-list'.  It is a list of regexp
  values, and any positive match will eliminate the buffer from centaur
  tab's grouping collage."
    (if (null windows-and-tabs-buffer-filter-regexp-list)
	(default-centaur-tabs-hide-tab x)
	(let ((name (format "%s" x)))
	  (cl-loop for buffer-regexp-value
		  in windows-and-tabs-buffer-filter-regexp-list
		    if (string-match buffer-regexp-value name)
			return t
		    finally
			return nil))))

  )


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
          treemacs-no-png-images		 nil
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

    ;; use all-the-icons with treemacs
    (use-package treemacs-all-the-icons
      :after (treemacs all-the-icons)
      ;; :config (treemacs-load-theme "treemacs-all-the-icons")
      )

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    ;; actions to perform with the tab key (open files in new tab)
    (setq treemacs-TAB-actions-config
	  '((root-node-open . treemacs-toggle-node)
	   (root-node-closed . treemacs-toggle-node)
	   (dir-node-open . treemacs-toggle-node)
	   (dir-node-closed . treemacs-toggle-node)
	   (file-node-open . treemacs-visit-node-ace-horizontal-split)
	   (file-node-closed . treemacs-visit-node-ace-horizontal-split)
	   (tag-node-open . treemacs-visit-node-ace-horizontal-split)
	   (tag-node-closed . treemacs-visit-node-ace-horizontal-split)
	   (tag-node . treemacs-visit-node-ace-horizontal-split)))

    ;; actions to perform with the return key (open files in vsplit)
    (setq treemacs-RET-actions-config
	  '((root-node-open . treemacs-toggle-node)
	   (root-node-closed . treemacs-toggle-node)
	   (dir-node-open . treemacs-toggle-node)
	   (dir-node-closed . treemacs-toggle-node)
	   (file-node-open . treemacs-visit-node-ace)
	   (file-node-closed . treemacs-visit-node-ace)
	   (tag-node-open . treemacs-visit-node-ace)
	   (tag-node-closed . treemacs-visit-node-ace)
	   (tag-node . treemacs-visit-node-ace)))

    ;; enable modes
    (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

  ))

;; evil mode support in treemacs
(use-package treemacs-evil
  :after treemacs evil)

;; change treemacs workspace when jumping to tab by number
(defun jump-tab-and-workspace (&optional tabnum)
  "Switch tab-bar tab and treemacs workspace.

The assumption is that there is a Treemacs workspace corresponding to
the tab-bar name.

The TABNUM argument is a prefix passed on to `tab-bar-select-tab'"
  (interactive "P")
  (when tabnum
    (tab-bar-select-tab tabnum)
    ;; (switch-to-workspace-by-tab-name)
    (treemacs-do-switch-workspace
      (cdar (cdr (tab-bar-get-buffer-tab (current-buffer)))))
    )
  )

;; change treemacs workspace when jumping to next / previous tab
(defun next-tab-and-workspace ()
  "Switch to next tab-bar tab and change treemacs workspace."
  (interactive)
  (tab-bar-switch-to-next-tab)
  (treemacs-do-switch-workspace
   (cdar (cdr (tab-bar-get-buffer-tab (current-buffer)))))
  ;; (switch-to-workspace-by-tab-name)
  )
(defun previous-tab-and-workspace ()
  "Switch to previous tab-bar tab and change treemacs workspace."
  (interactive)
  (tab-bar-switch-to-prev-tab)
  (treemacs-do-switch-workspace
   (cdar (cdr (tab-bar-get-buffer-tab (current-buffer)))))
  ;; (switch-to-workspace-by-tab-name)
  )



(provide 'windows-and-tabs)
;;; windows-and-tabs.el ends here
