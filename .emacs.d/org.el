;;; org.el --- Org mode customizations -*- lexical-binding: t -*-

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

;; Keymaps and extensions for Org mode

;;; Code:

;; group under "dbargman/org"
(defgroup dbargman/org nil
  "Personalized settings for org mode."
  :group 'External
  :prefix 'dbargman/org
  :version '0.1.0)

;; path to capture templates (context-specific)
(defcustom dbargman/org-capture-template-dir
  "~/.org-mode/capture-templates"
  "directory where org capture template files can be found."
  :group 'dbargman/org
  :type 'string
  :safe (lambda (_) t))

;; org roam filetags to use for agenda files
(defcustom dbargman/org-roam-node-agenda-tags (list "inbox")
  "Nodes with these '#+filetags' are added to org-agenda-files."
  :group 'dbargman/org
  :type 'list
  :safe (lambda (_) t))


;;; DEPENDENCIES

;; org mode itself
(straight-use-package '(org :type built-in)

  :config


  ;; prettify check boxes
  (add-hook 'org-mode-hook (lambda ()
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

  ;; indent automaticall
  (add-hook 'org-mode-hook 'org-indent-mode)

  )

;; evil keybindings
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; editing of org source blocks
(use-package company-org-block
  :ensure t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook (org-mode
	 . (lambda ()
	     (make-local-variable 'company-backends)
	     (add-to-list
	      'company-backends
	      (company-mode/backend-with-yas 'company-org-block)
	      )
	     )
	 )

  )

;; ;;; reference management
;; (use-package org-ref)

;;; org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory
   "~/dotfile-notes/roam")
  (org-roam-dailies-directory
   "journal/")
  (org-roam-db-location
   "~/.org-mode/roam-db-files/dotfiles.db")
  (org-roam-completion-everywhere t)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-enable)

  )

;; consult integration
(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; ;; Display org-roam buffers right after non-org-roam buffers
   ;; ;; in consult-buffer (and not down at the bottom)
   ;; (consult-org-roam-buffer-after-buffers t)

   :config
   ;; suppress previewing for certain functions
   (consult-customize
    org-roam-node-find
    org-roam-node-insert
    consult-org-roam-file-find
    consult-org-roam-forward-links
    :preview-key "M-.")
   ;; :bind
   ;; ;; Define some convenient keybindings as an addition
   ;; ("C-c n e" . consult-org-roam-file-find)
   ;; ("C-c n b" . consult-org-roam-backlinks)
   ;; ("C-c n l" . consult-org-roam-forward-links)
   ;; ("C-c n r" . consult-org-roam-search)
   )

;;; org-babel

;; languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   )
 )


;;; AESTHETICS

;; default view
(setq
 org-startup-indented t
 ;; org-pretty-entities t
 ;; org-hide-emphasis-markers t
 org-startup-with-inline-images t
 org-image-actual-width '(300)
 )

;; org-agenda settings
(setq

 ;; view in tabular mode by default
 org-agenda-view-columns-initially t

 ;; default column format
 org-columns-default-format-for-agenda " %3PRIORITY %TODO %50ITEM %25TAGS"

 ;; always show file tags in the agenda buffer
 org-agenda-show-inherited-tags 'always

 ;; always inherit file-level properties
 org-use-property-inheritance t

)
(custom-set-variables
 '(org-priority-highest 1)
 '(org-priority-lowest 7)
 '(org-priority-default 1)
 )


;; prettify org-mode bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list
   '(("●" 9673) ("◉" 9673) ("○" 9673) ("•" 9673)))
  :config
  (org-superstar-configure-like-org-bullets)
  )


;;; FUNCTIONALITY

;; helper; shorthand for exporting to PDF without extra clutter

;; restart org-mode in all org buffers
(defun dbargman/global-org-mode-restart ()
  "Run 'org-mode-restart' in all open Org mode buffers."
  (save-window-excursion
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (string-equal major-mode "org-mode")
	  (org-mode-restart)
	  )
	)
      )
    )
  )

;; restart org mode after capturing from template
(add-hook 'org-capture-after-finalize-hook
	  'dbargman/global-org-mode-restart)

;; helper: get string from file
(defun dbargman/contents-of-file (filePath)
  "Return contents of file under FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; get node file from node title (or alias)
(defun dbargman/org-roam-get-node-file (node-title)
  "Get full file path to an org roam node NODE-TITLE file.

NODE-TITLE can be a title or an alias."
  (org-roam-node-file (org-roam-node-from-title-or-alias node-title)))

;; get a path to a capture template file
(defun dbargman/org-capture-get-template (name)
  "Get full path to a capture template file.

The template must be an Org file in 'dbargman/org-capture-template-dir'.
NAME specifies the template file name without the '.org' extension."
  (concat
   (file-name-as-directory
    (file-truename dbargman/org-capture-template-dir))
   name ".org"
   )
  )

;; list of org-roam nodes which should be added to org-agenda-files
(defun dbargman/org-roam-agenda-files ()
  "List of org-roam nodes that should be added to org-agenda-files.

This function returns a list of fully qualified filepaths to all nodes
whose #+filetags match one of 'dbargman/org-roam-node-agenda-tags'"

  ;; return the node's file path for each node
  (mapcar
   (lambda (node) (org-roam-node-file node))

   ;; filter org-roam nodes by tag
   (seq-filter
    (lambda (node)
      (let (flag)
	;; for each agenda tag, add the node if it has it
	(dolist (tag dbargman/org-roam-node-agenda-tags flag)
	  (when (member tag (org-roam-node-tags node))
	    (setq flag t)
	    )
	  )
	flag
	)
      )
    (org-roam-node-list)
    )
   )
  )

;; add project tag to an org-roam-node programmatically
(defun org-roam-node-project-tag (_)
  "Interactively select a project tag in an org-roam node template.

Adding '${project-tag}' or '${project-tag-lower}' (for lower-case) into
an org-roam capture template should trigger a 'completing-read'
interface with candidates pre-populated from the value of
'dbargman/org-roam-node-agenda-tags'. When the capture template is
called from within an active project node, the input will be
prepopulated with the (first) agenda filetag of the source node."
  (defvar-local node-project-tag nil)
  (defvar-local node-project-tag-lower nil)
  (or node-project-tag
      (progn
	(setq-local
	 node-project-tag
	 (completing-read
	  "Enter a project tag for the node: "
	  dbargman/org-roam-node-agenda-tags
	  )
	 )
	(setq-local node-project-tag-lower (downcase node-project-tag))
	node-project-tag)
      )
  )

;; lower-case version of the project tag
(defun org-roam-node-project-tag-lower (node)
  (progn (funcall 'org-roam-node-project-tag node) node-project-tag-lower))


;; run a non-interactive capture to an org-roam node
(defun dbargman/org-roam-capture-in-background (template info)
  "Capture INFO in the background using TEMPLATE.

INFO must be a plist to pass to 'org-roam-capture-'.

TEMPLATE must be a valid org roam capture template, as defined in
'org-roam-capture-templates'. This function will automatically add an
':immediate-finish' and ':kill-buffer' properties to the template spec.
No further checks will be performed to ensure that INFO contains all the
relevant information to fill out the template..

"
  (org-roam-capture-
   :node (org-roam-node-create)
   :templates (list (append template '(:immediate-finish :kill-buffer)))
   :goto nil
   :info info
   )

  )

;; refile to a journal entry for the date specified by the entry's
;; active timestamp.
(defun dbargman/org-roam-refile-to-daily ()
  "Refile agenda entry at point to an 'org-roam-daily' file,

The refile behaviour can be adjusted by setting custom properties for
the entry. Here is an example configuration:

  :PROPERTIES:
  :refile/template: r
  :refile/target-date:      <YYYY-MM-DD>
  :refile/unset-todo-state: [ any except \"nil\" ]
  :refile/remove-tags:      [ any except \"nil\" ]
  :refile/keep-original:    [ any except \"nil\" ]
  :END:

'refile/template' must be a KEY pointing to an existing item from
'org-roam-dailies-capture-templates'. If not specified, the first
available template is used.

'refile/target-date' can be used to override the date under which the
entry is refiled. By default, today's date is used.

Adding the property key 'refile/unset-todo-state' unsets the entry's
TODO state,

Adding the property key 'refile/remove-tags' results in entry tags being
cleared before the entry is refiled.

Adding the property key 'refile/keep-original' results in the original
entry being kept in-place as a copy is refiled. By default, the original
entry is removed.

The values of boolean properties may be left empty. Explicitly setting a
property value to nil is the same as removing the property key."
  (interactive)
  (let (
	;; locally reduce list of capture templates to either the one
	;; with the 'refile/template' key, or the first one. Also make
	;; sure the template doesn't envoke an interactive prompt or
	;; leave a lingering buffer.
	(org-roam-dailies-capture-templates
	 (mapcar
	  '(lambda (el) (append el '(:immediate-finish :kill-buffer)))
	  (or 
	   (seq-filter
	    (lambda (el)
	      (when
		  (string-equal
		   (car el)
		   (org-entry-get nil "refile/template")
		   )
		el
		)
	      )
	    org-roam-dailies-capture-templates
	    )
	   `(,(car org-roam-dailies-capture-templates))
	   )
	  )
	 )

	;; unset todo state if 'refile/unset-todo-state' is present
	(unset-todo-state (org-entry-get nil "refile/unset-todo-state"))

	;; refile date, and how to treat tags
	(target-date (org-entry-get nil "refile/target-date"))
	(remove-tags (org-entry-get nil "refile/remove-tags"))

	;; keep original entry, if requested
	(org-refile-keep
	 (if (org-entry-get nil "refile/keep-original") t nil))

	;; pre-allocate some variables we'll need for later
	daily-file-name
	daily-file-buffer
	pos

	;; set necessary refile properties locally
	(org-refile-use-outline-path nil)
	(org-after-refile-insert-hook #'save-buffer)

	)

    ;; create daily file if it doesn't exist
    (save-window-excursion
      (org-roam-dailies--capture
       (if target-date (date-to-time target-date) (current-time)) t)
      (setq daily-file-buffer (current-buffer))
      (setq daily-file-name (buffer-file-name))
      (evil-newline-after)
      (setq pos (point))
      )

    ;; unset todo state and remove tags as requested
    (when unset-todo-state (org-todo ""))
    (when remove-tags (org-set-tags nil))

    ;; refile and kill target buffer
    (org-refile nil nil (list nil daily-file-name nil pos))
    )
  )


(provide 'org)

;;; org.el ends here
