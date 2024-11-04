;;; ecm.el --- Context management -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Emacs desktop-save-mode allows the user to choose a custom location
;; of the desktop file via a local variable. This package extends this
;; functionality with a prompt which allows the user to choose the
;; location of the session file at startup.

;;; Code:

;; helper function: create session config
(defun ecm--make-session-config
    (desktop-file-dir
     &optional
     org-roam-dir
     org-roam-dailies-dir
     org-roam-db-file
     mu4e-context)
  "Create a hash table with session configurations.

DESKTOP-FILE-DIR specifies the path (as a string) where the desktop file
will be maintained.

ORG-ROAM-DIR is an optional string specifying the path to the org-roam
database for the session.

ORG-ROAM-DAILIES-DIR is an optional string specifying a subfolder name
within org-roam-dir to use for 'org-roam-dailies'.

ORG-ROAM-DB-FILE is a string specifying the full path for the custom
org-roam database file if a custom ORG-ROAM-DIR has been specified.

MU4E-CONTEXT is an optional string specifying the default email context
for the session.

"
  (let ((session-config (make-hash-table :test 'equal :size 5)))
    (puthash 'desktop-file-dir desktop-file-dir session-config)
    (puthash 'org-roam-dir org-roam-dir session-config) 
    (puthash 'org-roam-dailies-dir org-roam-dailies-dir session-config) 
    (puthash 'org-roam-db-file org-roam-db-file session-config) 
    (puthash 'mu4e-context mu4e-context session-config)
    session-config
    )
  )


;; group under "ecm"
(defgroup ecm nil
  "Settings for advanced context config."
  :group 'External
  :prefix 'ecm
  :version '0.1.0)

;; additional session environment variables
(defcustom ecm-default-session-name nil
  "Default session name to use when launching Emacs.

When nil, an interactive prompt appers at each startup.

"
  :group 'ecm
  :type 'symbol
  :safe (lambda (_) t))

;; custom variable for storing session configurations
(defvar ecm--disable-sessions 'disable\ session\ support)
(defcustom ecm--sessions
  `(
    (,ecm--disable-sessions . ,(ecm--make-session-config ""))
    )
  "Variable for storing the user's session configurations."
  :group 'ecm
  :type '(alist
	  :key-type (symbol :tag "session name")
	  :value-type (hash-table :tag "session config")
	  )
  :safe (lambda (_) t)
  )

;; additional session environment variables
(defcustom ecm-session-init-commands nil
  "Commands to execute when a session is initialized.

Alist linking a session name to a list of commands that should be
executed during 'ecm-initialize-session'..

"
  :group 'ecm
  :type 'alist
  :safe (lambda (_) t))


;; other variables that need defaults for the benefit of this module
(defvar org-roam-directory "")
(defvar ecm--email-contexts (list "no default context"))
(defvar ecm--active-session-name nil)

(defun check-prefix (arg)
  (interactive P)
  (type-of arg))

;; interactively choose desktop session
(defun ecm-session-init (&optional arg)
  "Choose a desktop session or create a new session file interactively.

When running with a prefix ARG, the user will be prompted to set the
session's context configuration interactively even if the session is
already defined. Previous settings will be overwritten."
  (interactive "P")
  (let*
      (
       ;; prompt for existing or new session name
       (session-name
	(or ecm-default-session-name
	    (intern
	     (completing-read
	      (concat "Choose or create a session context "
		      "(or 'disable session support'): ")
	      ecm--sessions)
	     )
	    )
	)

       ;; existing session config, if an existing session was chosen and
       ;; no prefix argument is specified.
       (existing
	(if (and arg (not (equal session-name ecm--disable-sessions)))
	    nil (alist-get session-name ecm--sessions)))

       ;; session directory (extract or prompt)
       (session-dir
	(if existing (gethash 'desktop-file-dir existing)
	  (read-file-name
	   "Specify a directory to store the '.emacs.desktop' file:"
	   nil nil t nil 'file-directory-p)
	  )
	)

       ;; prompt for whether to use custom org-roam project
       (custom-org-roam-p
	(if existing t (y-or-n-p "Use dedicated org-roam instance?")))

       ;; org roam directory (extract or prompt)
       (org-roam-dir
	(if existing (gethash 'org-roam-dir existing)
	  (when custom-org-roam-p
	    (file-name-as-directory
	     (file-truename
	      (read-file-name
	       "Specify the project's org-roam-directory: "
	       nil nil 'confirm nil 'file-directory-p)
	      )
	     )
	    )
	  )
	)

       ;; org roam dailies folder (extract or prompt)
       (org-roam-dailies-dir
	(if existing (gethash 'org-roam-dailies-dir existing)
	  (when custom-org-roam-p
	    (file-name-as-directory
	     (completing-read
	      "Specify a subfolder name to use for org-roam-dailies: "
	      nil nil nil "daily")
	     )
	    )
	  )
	)

       ;; org-roam database file (extract or prompt)
       (org-roam-db-file
	(if existing (gethash 'org-roam-db-file existing)
	  (when custom-org-roam-p
	    (read-file-name
	     "Specify the project's org-roam-db-location: "
	     (concat
	      (file-name-as-directory
	       (file-name-parent-directory org-roam-dir))
	      "org-roam.db")
	     nil 'confirm nil 'file-writable-p)
	    )
	  )
	)

       ;; context names, and choice of email context
       (mu4e-context
	(if existing (gethash 'mu4e-context existing)
	  (let
	      ((email-context-names
		(let ((context-names ecm--email-contexts))
		  (dolist (context mu4e-contexts context-names)
		    (setq
		     context-names
		     (add-to-list
		      'context-names
		      (cl-struct-slot-value 'mu4e-context 'name context)
		      t)))
		  context-names)
		))
	    (completing-read
	     "Choose email context for this session (optional): "
	     email-context-names nil t nil (car email-context-names)
	     )
	    )
	  )
	)
       )

    ;; if running with no session
    (desktop-save-mode -1)
    (if (eq session-name ecm--disable-sessions)

	;; if running without sessions, disable desktop file and use
	;; defaults elsewhere
	(progn
	  (setq ecm--active-session-name ecm--disable-sessions)
	  (setq desktop-path (list))
	  )

      ;; otherwise enable session-specific settings

      ;; update candidate list
      (customize-save-variable
       'ecm--sessions
       (cl-delete-duplicates
	(append
	 `(
	   (,session-name
	    . ,(ecm--make-session-config session-dir
					 org-roam-dir
					 org-roam-dailies-dir
					 org-roam-db-file
					 mu4e-context)
	    )
	   )
	 ecm--sessions)
	:test '(lambda (c1 c2) (string-equal (car c1) (car c2)))
	:from-end t
	)
       )

      ;; switch to a mu4e context, if set
      (unless (string-equal mu4e-context (car ecm--email-contexts))
	(mu4e-context-switch nil mu4e-context))

      ;; switch to an org roam directory, if set
      (when org-roam-dir
	(progn
	  (make-directory (concat org-roam-dir org-roam-dailies-dir) t)
	  (make-directory (file-name-parent-directory org-roam-db-file) t)
	  (setq org-roam-directory org-roam-dir)
	  (setq org-roam-dailies-directory org-roam-dailies-dir)
	  (setq org-roam-db-location (file-truename org-roam-db-file))
	  (org-roam-db-autosync-enable)
	  )
	)

      ;; enable session support in the specified directory
      (setq desktop-path (list session-dir))
      (desktop-change-dir session-dir)
      (desktop-save-mode t)

      ;; run init commands, if defined
      (let
	  ((commands (alist-get session-name ecm-session-init-commands))
	   )
	(when commands (funcall (eval (append '(lambda ()) commands)))))

      ;; finally, save name of current active session as a variable
      (setq ecm--active-session-name session-name)

      )
    )
  )

;; interactively forget desktop session
(defun ecm-session-forget ()
  "Choose a desktop session or create a new session file interactively."
  (interactive)
  (let*
      (

       ;; prompt for existing or new session name
       (session-name
	(intern
	 (completing-read
	  "Forget session:"
	  ecm--sessions
	  (lambda (el) (not (eq (car el) ecm--disable-sessions)))
	  t)
	 )
	)

       ;; extract session file path and name
       (session-path
	(gethash "desktop-file-dir"
		 (alist-get session-name ecm--sessions)
		 )
	)
       (session-file (expand-file-name ".emacs.desktop" session-path))

       ;; if session name new, prompt for a file; otherwise extract
       (delete-file-p
	(when session-name (y-or-n-p "Delete desktop file (if found)?"))
	)

       )

    ;; if forgetting active session, disable session support
    (when (eq session-path (car desktop-path)) (desktop-save-mode -1))

    ;; delete session file (if found)
    (when delete-file-p (delete-file session-file))

    ;; delete session entry
    (customize-save-variable
     'ecm--sessions
     (assq-delete-all session-name ecm--sessions)
     )

    )
  )


(provide 'ecm)

;;; ecm.el ends here
