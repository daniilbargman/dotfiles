;;; email.el --- email client configuration -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: daniil.bargman@gmail.com
;; Version: 0.1.0
;; Package-Requires: (getmail, mu, mu43)
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

;; Configurations for mu4e email client in Emacs.  Here, only the basic
;; settings specifying the client's behaviour are configured.  Settings
;; email accounts and contexts are delegated to startup-scripts.el.
;;
;; This module required getmail, mu, and mu4e.  Getmail is available in
;; the apt repo.  Mu and mu4e can be installed using mu4e-setup.sh.
;;
;; It is assumed that at least one email account has been configured as
;; a Maildir, and that the mu backend has been initialized in advance
;; (see the mu4e user manual).

;;; Code:

;; (based on defaults copied from mu4e manual)

;; NOTE as of 1.8 mu4e is installed in /usr/local/share/emacs/site-lisp,
;; which is on the path
;(add-to-list 'load-path "~/.emacs.d/mu/mu4e")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;;; CUSTOMIZATIONS

;; group under "advanced-session"
(defgroup dbargman/email-settings nil
  "Additional settings for mu4e."
  :group 'External
  :prefix 'dbargman/email
  :version '0.1.0)

;; additional session environment variables
(defcustom dbargman/email-capture-rules nil
  "Alist of rules for automatically capturing emails.

It is consumed by 'dbargman/email-auto-capture'"
  :group 'dbargman/email
  :type 'alist
  :safe (lambda (_) t))

;;; GOLBAL SETTINGS

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent

      ;; point to mu binary
      mu4e-mu-binary "/usr/local/bin/mu"

      ;; make sure to flow text
      mu4e-compose-format-flowed t

      ;; make mu4e play better with mbsync by renaming moved files
      mu4e-change-filenames-when-moving t

      ;; don't show update warnings
      mu4e-index-update-error-warning nil

      ;; don't show "Indexing..." message
      mu4e-hide-index-messages t

      ;; the headers to show in the headers list -- a pair of a field
      ;; and its width, with `nil' meaning 'unlimited'
      ;; (better only use that for the last field.
      ;; These are the defaults:
      mu4e-headers-fields
	  '( (:human-date	   .  10)
	    (:flags		   .   4)
	    (:from		   .  20)
	    (:thread-subject       .  nil))

      ;; split window vertically when opening messages
      ;; mu4e-split-view 'vertical
      mu4e-split-view nil
      mu4e-headers-visible-columns 70

      ;; use fancy characters in mu4e
      mu4e-use-fancy-chars t

      ;; prefer HTML body over plaintext body, when available
      mu4e-view-prefer-html t

      ;; don't keep message buffers around
      message-kill-buffer-on-exit t

      ;; smtp settings
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl

      ;; ;; mail update command
      ;; mu4e-get-mail-command "mbsync -a"

      ;; context policy: only pick if none
      mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'ask-if-none

      )


;;; ORG-MSG and HTML support

;;; integrate with org-msg
(use-package org-msg
  :requires (emacs-htmlize))

;; enable org-msg-mode and configure for mu4e
(org-msg-mode)
(org-msg-mode-mu4e)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
    org-msg-startup "hidestars indent inlineimages"
    org-msg-greeting-fmt "\nHi *%s*,\n\n"
    org-msg-greeting-name-limit 3
    org-msg-default-alternatives '((new html) (reply-to-html html))
    org-msg-convert-citation t
    )


;;; CUSTOMIZATIONS IN THE MAIN VIEW`'

;; generic function for setting maildir shortcuts on entering a context
(defvar dbargman/inbox-folder "")
(defun dbargman/prepend-maildir (query-string)
  "Prepend maildir as filter to email query set by QUERY-STRING.

Adds \"maildir:<dbargman/inbox-folder>\" at the beginning of the query."
  (concat "maildir:" dbargman/inbox-folder " " query-string))
(defun dbargman/configure-mu4e-shortcuts ()
  "Function for setting maildir shortcuts for the main folders.

It works by linking the shortcuts to the value of the dynamically set
maildir variables, rather than by hardcoding maildir paths into the
shortcut spec.

The applied mapping looks like the default setup, i.e.:

    mu4e-sent-folder    ?s
    mu4e-drafts-folder  ?d
    mu4e-refile-folder  ?a
    mu4e-trash-folder   ?t

In addition, a shortcut is mapped against the inbox folder specified by
a custom variable:

    dbargman/inbox-folder     ?i

NOTE: setting any of the above folder variables to \"/\" will disable
the corresponding shortcut.

In addition, configures mu4e-bookmarks to restrict their output to the
maildir set by 'dbargman/inbox-folder'.

The value for 'dbargman/inbox-folder' can be set inside the ':vars' slot when
calling 'make-mu4e-context'."
  (setq mu4e-maildir-shortcuts (list))

  ;; only append folders that are not nil
  (unless (string-equal mu4e-trash-folder "/")
    (setq mu4e-maildir-shortcuts
	  (append `((:maildir ,mu4e-trash-folder :key ?t))
		  mu4e-maildir-shortcuts)))
  (unless (string-equal mu4e-refile-folder "/")
    (setq mu4e-maildir-shortcuts
	  (append `((:maildir ,mu4e-refile-folder :key ?a))
		  mu4e-maildir-shortcuts)))
  (unless (string-equal mu4e-drafts-folder "/")
    (setq mu4e-maildir-shortcuts
	  (append `((:maildir ,mu4e-drafts-folder :key ?d))
		  mu4e-maildir-shortcuts)))
  (unless (string-equal mu4e-sent-folder "/")
    (setq mu4e-maildir-shortcuts
	  (append `((:maildir ,mu4e-sent-folder :key ?s))
		  mu4e-maildir-shortcuts)))
  (unless (string-equal dbargman/inbox-folder "/")
    (setq mu4e-maildir-shortcuts
	  (append `((:maildir ,dbargman/inbox-folder  :key ?i))
		  mu4e-maildir-shortcuts)))


  ;; restrict mu4e-bookmarks to current maildir 
  (setq mu4e-bookmarks
	`(( :name  "Unread messages"
	    :query ,(dbargman/prepend-maildir
		     "flag:unread AND NOT flag:trashed")
	    :key ?u)
	  ( :name "Today's messages"
	    :query ,(dbargman/prepend-maildir "date:today..now")
	    :key ?t)
	  ( :name "Last 7 days"
	    :query ,(dbargman/prepend-maildir "date:7d..now")
	    :hide-unread t
	    :key ?w)
	  ( :name "Messages with images"
	    :query ,(dbargman/prepend-maildir "mime:image/*")
	    :key ?p)
	  )
	)

  ;; Reload mu4e so the main view is refreshed with the new functions
  (mu4e)
  )

;; Use hook to conigure maildir shortcuts whenever a context changes.
;; It can't be put into :enter-func for mu4e-contexts because it relies
;; on values from the :vars field (:enter-func runs before :vars).
(add-hook 'mu4e-context-changed-hook
	  'dbargman/configure-mu4e-shortcuts)


;;; FUNCTIONS FOR CAPTURING EMAILS INTO ORG FILES

;; function for refiling specific incoming email directly to a
;; dedicated Org file for a project
(defun dbargman/email-org-capture (match-func capture-func
					&optional mark-read refile)
  "Capture unread emails into org files from the header view.

MATCH-FUNC is a test function that should take a message from the
headers view as an input and return t if the message should be captured.

CAPTURE-FUNC should usually be a function based on 'org-capture' or
'org-roam-capture'. It shouldn't require any arguments, but it can use
org capture fields provided by email message objects (e.g. '%:subject').

If MARK-READ is non-nil, captured messages are marked as read.

If REFILE is t, captured messages are refiled to 'mu4e-refile-folder'.
REFILE can also be set to a string specifying a custom refile location."

  ;; for each message, apply function
  (mu4e-headers-for-each
   (lambda (msg)
     (when (funcall match-func msg)
       (funcall capture-func)
       (when mark-read
	 (mu4e-mark-at-point 'read nil)
	 (mu4e-mark-execute-all t)
	 )
       (when refile
	 (mu4e-mark-at-point
	  'refile (if (booleanp refile) mu4e-refile-folder refile))
	 (mu4e-mark-execute-all t)
	 )
       )
     )
   )
  )

;; automatically capture emails into org files using
;; dbargman/email-capture-rules
(defun dbargman/capture-emails-quietly ()
  "Quietly capture emails using 'dbargman/email-capture-rules'."
  (interactive)
  (save-window-excursion

    ;; apply capture rules
    (dolist (caprule dbargman/email-capture-rules nil)
      (let
	  (
	   (match-func (alist-get 'match-func caprule))
	   (capture-func (alist-get 'capture-func caprule))
	   (mark-read (alist-get 'mark-read caprule))
	   (refile (alist-get 'refile caprule))
	   )
	(dbargman/email-org-capture
	 match-func capture-func mark-read refile)
	)
      )
    )
  )

;; capture opened email into org-roam node
(defun dbargman/capture-email-to-org-roam-node (&optional ARG)
  "Capture the contents of an email into an org-roam node.

Information about the email is passed to the capture function.
Specifically, the email subject can be used for the node's 'title' field
and an additional field 'email-body' can be used to render the email's
contents using '${email-body}' inside the template.

This function will invoke a 'completing-read' prompt for the node's
title with the email's subject inserted as the defualt suggestion. To
skip the 'completing-read' prompt and use the email's subject for the
title verbatim, call with a prefix argument ARG.

This function only works when the email is onpen in 'mu4e-view-mode'."

  (interactive "P")

  ;; add hook to mu4e view mode
  (when (eq major-mode 'mu4e-view-mode)
    (let* ((email-title
	   (if ARG (mu4e-message-field-at-point :subject)
	     (completing-read
	      "Please enter a title for the node: "
	       nil nil nil (mu4e-message-field-at-point :subject)
	      )
	     )
	   )
	   (email-link
	    (concat
	     "[[mu4e:msgid:" (mu4e-message-field-at-point :message-id)
	     "][email note]]"
	     )
	    )
	  (email-body
	   (save-excursion
	     (evil-goto-first-line)
	     (evil-forward-paragraph)
	     (buffer-substring (point) (point-max))
	     )
	   )
	  (org-roam-capture-new-node-hook
	   `(lambda () (org-roam-ref-add ,email-link)))
	  )
      email-link
      (org-roam-capture-
       :node (org-roam-node-create :title email-title)
       :info `(:title ,email-title :email-body ,email-body)
       )
      )
    )
  )

(provide 'email)
;;; email.el ends here
