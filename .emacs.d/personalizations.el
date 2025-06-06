;;; personalizations.el --- startup scripts -*- lexical-binding: t -*-

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

;; Startup scripts for Emacs.  This file may need to be loaded as early
;; as possible, so "with-eval-after-load" clauses are encouraged.

;;; Code:


;; mu4e contexts
(setq
 mu4e-contexts
 `(

   ;; statosphere - personal email
   ,(make-mu4e-context
     :name "statosphere-personal"
     :enter-func (lambda () (mu4e-message
			     "daniil.bargman@statosphere.com"))
     ;; :leave-func (lambda () (mu4e-message "eaving Private context"))
     ;; we match based on the contact-fields of the message
     :match-func
     (lambda (msg)
       (when msg
	 (string-match-p "^/statosphere/daniil.bargman/Inbox"
			 (mu4e-message-field msg :maildir)))
       ;; (mu4e-message-contact-field-matches
       ;;  msg '(:to :cc :bcc) "daniil.bargman@statosphere.com"))
       )

     :vars
     `(
       (mu4e-get-mail-command
	. "mbsync -a -c /mnt/projects/statosphere/admin/email/.mbsyncrc")
       (user-mail-address . "daniil.bargman@statosphere.com")
       (user-full-name . "Daniil Bargman")
       (message-user-organization . "statOsphere")
       (org-msg-signature
	.
	,(dbargman/contents-of-file
	  "/mnt/projects/statosphere/admin/email/default-signature.html"))
       ;; (mu4e-compose-reply-to-address . "daniil.bargman@statosphere.com")
       (smtpmail-default-smtp-server . "mail.statosphere.com")
       (smtpmail-smtp-server . "mail.statosphere.com")
       (smtpmail-local-domain . "statosphere.com")
       (smtpmail-smtp-user . "daniil.bargman@statosphere.com")
       (smtpmail-stream-type . starttls)
       (smtpmail-smtp-service . 587)
       (mu4e-sent-folder . "/statosphere/daniil.bargman/Sent")
       (mu4e-drafts-folder . "/statosphere/daniil.bargman/Drafts")
       (mu4e-trash-folder . "/statosphere/daniil.bargman/Trash")
       (mu4e-refile-folder . "/statosphere/daniil.bargman/Archive")
       (dbargman/inbox-folder . "/statosphere/daniil.bargman/Inbox")
       )
     )

   ;; UCL email
   ,(make-mu4e-context
     :name "UCL"
     :enter-func (lambda () (mu4e-message "UCL emails"))
     ;; :leave-func (lambda () (mu4e-message "eaving Private context"))
     ;; we match based on the contact-fields of the message
     :match-func
     (lambda (msg)
       (when msg
	 (string-match-p "^/UCL/daniil.bargman.22/Inbox"
			 (mu4e-message-field msg :maildir)))
       ;; (mu4e-message-contact-field-matches
       ;;  msg '(:to :cc :bcc) "daniil.bargman@statosphere.com"))
       )

     :vars
     `(
       (dbargman/email-oauth2ms-config-file
	. "UCL/ucesdb2@ucl.ac.uk-oauth2ms-config")
       (mu4e-get-mail-command . "mbsync -a -c ~/Research/.mbsyncrc")
       (user-mail-address . "daniil.bargman.22@ucl.ac.uk")
       (user-full-name . "Daniil Bargman")
       (message-user-organization . "UCL")
       ;; (org-msg-signature
       ;; 	.
       ;; 	,(dbargman/contents-of-file
       ;; 	  "/mnt/projects/statosphere/admin/email/default-signature.html"))
       ;; (mu4e-compose-reply-to-address . "daniil.bargman.22@ucl.ac.uk")
       (mu4e-attachment-dir . "~/Research/")
       ;; (send-mail-function . smtpmail-send-it)
       ;; (message-send-mail-function . smtpmail-send-it)
       (smtpmail-default-smtp-server . "smtp.office365.com")
       (smtpmail-smtp-server . "smtp.office365.com")
       (smtpmail-local-domain . "ucl.ac.uk")
       (smtpmail-smtp-user . "ucesdb2@ucl.ac.uk")
       (smtpmail-stream-type . starttls)
       (smtpmail-smtp-service . 587)
       (smtpmail-debug-info t)
       (smtpmail-debug-verb t)
       ;; (smtpmail-local-domain . "statosphere.com")
       (mu4e-sent-folder . "/UCL/daniil.bargman.22/Sent Items")
       (mu4e-drafts-folder . "/UCL/daniil.bargman.22/Drafts")
       (mu4e-trash-folder . "/UCL/daniil.bargman.22/Deleted Items")
       (mu4e-refile-folder . "/UCL/daniil.bargman.22/Archive")
       (dbargman/inbox-folder . "/UCL/daniil.bargman.22/Inbox")

       ;; define a custom CSS file for email message HTML rendering
       (org-msg-enforce-css . "~/Research/email-format.css")
       )
     )

   ;; notes-to-self from Gmail
   ,(make-mu4e-context
     :name "Notes from Gmail"
     :enter-func (lambda () (mu4e-message
			     "Notes from Gmail"))
     ;; :leave-func (lambda () (mu4e-message "leaving Private context"))
     ;; we match based on the contact-fields of the message
     :match-func
     (lambda (msg)
       (when msg
	 (string-match-p "^/personal/Gmail/Notes"
			 (mu4e-message-field msg :maildir)))
       )

     :vars
     `(
       (mu4e-get-mail-command . "mbsync -a -c ~/Email/personal/.mbsyncrc")
       (user-mail-address . "daniil.bargman@gmail.com")
       (user-full-name . "Daniil Bargman")
       ;; (mu4e-compose-reply-to-address . "daniil.bargman@gmail.com")
       (smtpmail-default-smtp-server . "imap.gmail.com")
       (smtpmail-smtp-server . "mail.gmail.com")
       ;; (smtpmail-local-domain . "statosphere.com")
       (mu4e-sent-folder . "/")	     ; setting to "/" hides the shortcut
       (mu4e-drafts-folder . "/")
       (mu4e-trash-folder . "/personal/Gmail/Trash")
       (mu4e-refile-folder . "/personal/Gmail/Archive")
       (dbargman/inbox-folder . "/personal/Gmail/Notes")
       )
     )
   )
 )

;; helper: match function and capture function for capturing emails into
;; org-roam inbox.org file using the email-note.org template based on
;; the email's subject prefix.
(defun dbargman/capture-rules-by-subject-prefix (prefix &optional tag
						  mark-read refile)
  "Helper: email capture config for dbargman/advanced-session-init-commands.

Create an alist entry for 'dbargman/email-capture-rules' for capturing emails

based on specific email prefixes.

Emails are captured into a special org-roam node called 'inbox.org'
using the template file 'email-note.org'.

PREFIX defines the regex test for the email's subject field. TAG is an
optional tag that can be added to the org entry.

MARK-READ and REFILE are passed on to 'dbargman/email-org-capture'."
  (let ((tag (or tag "")))
    `(
      (match-func
       . (lambda (msg)
	   (string-match ,prefix (mu4e-message-field msg :subject))))
      (capture-func
       . (lambda ()
	   (dbargman/org-roam-capture-in-background
	    `("e" "email note" entry
	      (file ,(dbargman/org-capture-get-template "email"))
	      :target (file+head "inbox.org" "#+title: inbox\n")
	      :prepend
	      :immediate-finish
	      :empty-lines-before 1
	      :empty-lines-after 1
	      )
	    '(:prefix ,prefix :tag ,tag)
	    )
	   )
       )
      (mark-read . ,mark-read)
      (refile . ,refile)
      )
    )
  )

;; session commands
(setq
 ecm-session-init-commands
 '(

   ;; session context for account management and system configuration
   (dotfiles
    . (

       ;; list of org-roam project filetags to add to org-agenda
       (setq
	dbargman/org-roam-node-agenda-tags
	'("inbox" "sttospr" "research" "emacs" "wm" "os" )
	)

       ;; org TODO keywords
       (setq
	org-todo-keywords
	'(

	  ;; refile labels for captured emails
	  (sequence "EMAIL" "|" "CAPTURED" "DISCARDED" "DONE")

	  ;; TODO flow for project tasks
	  (sequence "IDEA(i/!)" "TODO(t/!)" "|"
		    "DISCARDED(c/!)" "DONE(d/!)")
	  
	  )
	)

       ;; org agenda files
       (setq org-agenda-files (dbargman/org-roam-agenda-files))
       (add-hook
	'after-save-hook
	'(lambda ()
	   (when (string-equal major-mode "org-mode")
	     (setq org-agenda-files (dbargman/org-roam-agenda-files)))
	   )
	)

       ;; org agenda views
       (setq org-agenda-custom-commands
	     '(("n" "All TODOs by agenda tag"
		(
		 (todo "EMAIL")
		 (tags-todo "sttospr")
		 (tags-todo "research")
		 (tags-todo "emacs")
		 (tags-todo "wm")
		 (tags-todo "os")
		 )
		((org-agenda-sorting-strategy
		  '(todo-state-down priority-down)
		  )
		 (org-overriding-columns-format
		  " %3PRIORITY %TODO %50ITEM %25CATEGORY")
		 )
		)
	       )
	     )

       ;; capture templates for org-roam
       (setq

	;; capture into org-roam
	org-roam-capture-templates
	`(

	  ;; default template
	  ("g" "generic" plain "%?"
	   :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
	   :jump-to-captured
	   )

	  ;; project-linked node
	  ("t" "task" plain
	   (file ,(dbargman/org-capture-get-template "task"))
	   :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
	   :jump-to-captured
	   )

	  ;; project-linked node from email
	  ("e" "task from email" plain
	   (file ,(dbargman/org-capture-get-template
		   "task-from-email"))
	   :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
	   :jump-to-captured
	   )

	  )

	;; capture into dailies
	org-roam-dailies-capture-templates
	`(
	  ("e" "email note" entry "\n\n* Emails\n\n%?"
	   :target
	   (file+head+olp
	    "%<%Y-%m-%d>.org"
	    "#+title: %<%Y-%m-%d>\n"
	    ("Emails")
	    )
	   )
	  ("p" "project task"
	   entry "\n\n* Tasks\n\n%?"
	   :target
	   (file+head+olp
	    "%<%Y-%m-%d>.org"
	    "#+title: %<%Y-%m-%d>\n"
	    ("Tasks")
	    )
	   )
	  )
	)

       ;; rules for capturing emails from mu4e into org-roam
       (setq
	dbargman/email-capture-rules
	`(
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^dots: ?" "dotfiles" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^dotfiles: ?" "dotfiles" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^dots-res: ?" "dotfiles:research" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^dots-sttospr: ?" "dotfiles:statosphere" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^note: ?" "notes" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^NTS: ?" "notes" t t)
	  )
	)

       ;; refresh org-mode buffers
       (dbargman/global-org-mode-restart)

       )
    )

   ;; session context for research projects
   (research
    . (

;;; bibliography management

       ;; directory for bibliographic notes
       (custom-set-variables
	'(dbargman/research-markups-dir "markups")
	)

       ;; propagate the same directory to noter and citar
       (setq
	citar-org-roam-subdir dbargman/research-markups-dir
	org-noter-check-paths (expand-file-name
			       dbargman/research-markups-dir
			       org-roam-directory)
	)

       ;; make sure citar org roam uses the "m" template
       (setq citar-org-roam-capture-template-key "b")


;;; org-agenda settings

       ;; list of org-roam project filetags to add to org-agenda
       (setq
	dbargman/org-roam-node-agenda-tags
	'("inbox" "PhD" "PhD0" "PhD1" "PhD2" "PhD3"
	  "research" "philosophy" "misc")
	)

       ;; org agenda files
       (setq org-agenda-files (dbargman/org-roam-agenda-files))
       (add-hook
	'after-save-hook
	'(lambda ()
	   (when (string-equal major-mode "org-mode")
	     (setq org-agenda-files (dbargman/org-roam-agenda-files)))
	   )
	)

       ;; org TODO keywords
       (setq
	org-todo-keywords
	'(

	  ;; TODO flow for project tasks
	  (sequence "TODO(t/!)" "IDEA(i/!)" "|"
		    "DISCARDED(c/!)" "DONE(d/!)")

	  ;; refile labels for captured emails
	  (sequence "EMAIL" "|" "CAPTURED" "DISCARDED" "DONE")
	  
	  )
	)

       (setq
	org-columns-default-format-for-agenda " %9CATEGORY %85Agenda_Text "
	org-agenda-custom-commands
	'(
	  ("r" . "Research notes")
	  ("rp" "PhD notes"
	   (
	    (tags "+PhD1+LEVEL=1-scrapped")
	    (tags "+PhD1+LEVEL=1+scrapped")
	    (tags "PhD2+LEVEL=1-scrapped")
	    (tags "PhD2+LEVEL=1+scrapped")
	    (tags "PhD3+LEVEL=1-scrapped")
	    (tags "PhD3+LEVEL=1+scrapped")
	    (tags "PhD+LEVEL=1-scrapped")
	    (tags "PhD+LEVEL=1+scrapped")
	    )
	   (
	    (org-agenda-sorting-strategy '(category-up ts-down))
	    )
	   )
	  ("ro" "Other notes"
	   (
	    (tags "research")
	    (tags "philosophy")
	    (tags "misc")
	    )
	   (
	    (org-agenda-sorting-strategy '(category-down timestamp-down))
	    )
	   )
	  )
	)

       ;; capture templates for org-roam
       (setq
	org-roam-capture-templates
	`(

	  ;; index file for a new project
	  ("i" "unique index file for a new research project" plain
	   (file ,(dbargman/org-capture-get-template "project-master"))
	   :target
	   (file+head
	    "${title}.org"
	    ,(concat
	      "#+EXPORT_FILE_NAME: "
	      (
	       expand-file-name ".tmp/${slug}.tex"
	       dbargman/research-export-dir
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  ;; default template
	  ("r" "research note" plain
	   (file ,(dbargman/org-capture-get-template "research-note"))
	   :target
	   (file+head
	    "%<%Y%m%d%H%M%S>-${slug}.org"
	    ,(concat
	      "#+TITLE: ${title} | ${project-tag}: \n"
	      "#+SETUPFILE: "
	      (
	       expand-file-name "${project-tag-lower}.org"
	       org-roam-directory
	       )
	      "\n"
	      "#+EXPORT_FILE_NAME: "
	      (
	       expand-file-name
	       ".tmp/${project-tag-lower}-${slug}.tex"
	       dbargman/research-export-dir
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  ;; presentation
	  ("p" "presentation" plain
	   (file ,(dbargman/org-capture-get-template "research-slides"))
	   :target
	   (file+head
	    "%<%Y%m%d%H%M%S>-${slug}.org"
	    ,(concat
	      "#+EXPORT_FILE_NAME: "
	      (
	       expand-file-name
	       ".tmp/${project-tag-lower}-${slug}.tex"
	       dbargman/research-export-dir
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  ;; project-linked node from email
	  ("n" "note from email" plain
	   (file ,(dbargman/org-capture-get-template
		   "research-note-from-email"))
	   :target
	   (file+head
	    "%<%Y%m%d%H%M%S>-${slug}.org"
	    ,(concat
	      "#+TITLE: ${title} | ${project-tag}: \n"
	      "#+SETUPFILE: "
	      (
	       expand-file-name "${project-tag-lower}.org"
	       org-roam-directory
	       )
	      "\n"
	      "#+EXPORT_FILE_NAME: "
	      (
	       expand-file-name
	       ".tmp/${project-tag-lower}-${slug}.tex"
	       dbargman/research-export-dir
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  ;; bibliographic note
          ("b" "bibliographic note" plain
	   (file ,(dbargman/org-capture-get-template "bibliographic-note"))
	   :target
	   (file+head
	    ,(expand-file-name
	      "${slug}.org"
	      dbargman/research-markups-dir)
	    ,(concat
	      "#+SETUPFILE: "
	      (
	       expand-file-name "${project-tag-lower}.org"
	       org-roam-directory
	       )
	      "\n"
	      "#+EXPORT_FILE_NAME: "
	      (
	       expand-file-name
	       ".tmp/${project-tag-lower}-${slug}.tex"
	       dbargman/research-export-dir
	       )
	      "\n"
	      "#+PROPERTY: NOTER_DOCUMENT "
	      (
	       expand-file-name
	       "${citar-file}"
	       dbargman/research-bibliography-pdf-dir
	       )
	      )
	    )

           :unnarrowed t
	   :jump-to-captured

	   )

	  ;; project-linked node
	  ("t" "task" plain
	   (file ,(dbargman/org-capture-get-template "task"))
	   :target
	   (file+head
	    "%<%Y%m%d%H%M%S>-${slug}.org"
	    ,(concat
	      "#+SETUPFILE: "
	      (
	       expand-file-name "${project-tag-lower}.org"
	       org-roam-directory
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  ;; project-linked node from email
	  ("e" "task from email" plain
	   (file ,(dbargman/org-capture-get-template
		   "task-from-email"))
	   :target
	   (file+head
	    "%<%Y%m%d%H%M%S>-${slug}.org"
	    ,(concat
	      "#+SETUPFILE: "
	      (
	       expand-file-name "${project-tag-lower}.org"
	       org-roam-directory
	       )
	      )
	    )
	   :unnarrowed t
	   :jump-to-captured
	   )

	  )

	)

       ;; capture into dailies
       org-roam-dailies-capture-templates
       `(
	 ("e" "email refiled" entry "\n\n* Emails\n\n%?"
	  :target
	  (file+head+olp
	   "%<%Y-%m-%d>.org"
	   "#+title: %<%Y-%m-%d>\n"
	   ("Emails")
	   )
	  )

	 ;; progress notes
	 ("t" "task refiled" entry "\n\n* Activity log\n\n%?"
	  :target
	  (file+head+olp
	   "%<%Y-%m-%d>.org"
	   "#+title: %<%Y-%m-%d>\n"
	   ("Activity log")
	   )
	  )
	 )

       ;; rules for capturing emails from mu4e into org-roam
       (setq
	dbargman/email-capture-rules
	`(
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^res: ?" "research" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^phd: ?" "research:PhD:UCL" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^PhD0: ?" "research:PhD:UCL" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^sttospr: ?" "research:PhD:UCL" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^UCL: ?" "research:UCL" t t)
	  ,(dbargman/capture-rules-by-subject-prefix
	    "^phil: ?" "philosophy" t t)
	  )
	)

       ;; refresh org-mode buffers
       (dbargman/global-org-mode-restart)

       )
    )

 ;; session context for working on statosphere
 (statosphere
  . (

     ;; custom directory for capture templates
     (setq
      dbargman/org-capture-template-dir
      "/mnt/projects/statosphere/admin/.misc/org-capture-templates")

     ;; list of org-roam project filetags to add to org-agenda
     (setq
      dbargman/org-roam-node-agenda-tags
      '("inbox" "code" "infrastructure" "datasource" "business")
      )

     ;; org TODO keywords
     (setq
      org-todo-keywords
      '(

	;; refile labels for captured emails
	(sequence "EMAIL" "|" "CAPTURED" "DISCARDED(x@)" "DONE")

	;; TODO flow for project tasks
	(sequence "idea(i)" "note(n)" "TODO(t/!)" "|"
		  "DISCARDED(c@/!)" "DONE(d/!)")

	)
      )

     ;; org agenda files
     (setq org-agenda-files (dbargman/org-roam-agenda-files))
     (add-hook
      'after-save-hook
      '(lambda ()
	 (when (string-equal major-mode "org-mode")
	   (setq org-agenda-files (dbargman/org-roam-agenda-files)))
	 )
      )

     ;; org agenda views
     (setq
      org-columns-default-format-for-agenda " %3PRIORITY %TODO %50ITEM %25CATEGORY"
      org-agenda-custom-commands
      '(
	("t" . "Various filters for TODO categories")
	("tg" "Immediate TODOs for code base and infrastructure"
	 (
	  (todo "EMAIL")
	  (tags-todo "+code-TODOtree")
	  (tags-todo "+code+TODOtree")
	  (tags-todo "+infrastructure-TODOtree")
	  (tags-todo "+infrastructure+TODOtree")
	  )
	 ((org-agenda-sorting-strategy
	   '(todo-state-down priority-down timestamp-down)
	   )
	  )
	 )
	("te" todo "EMAIL")
	("tc" "Code base TODOs"
	 (
	  (tags-todo "+code-TODOtree")
	  (tags-todo "+code+TODOtree")
	  )
	 ((org-agenda-sorting-strategy
	   '(todo-state-down priority-down)
	   )
	  )
	 )
	("ti" "Infrastructure TODOs"
	 (
	  (tags-todo "+infrastructure-TODOtree")
	  (tags-todo "+infrastructure+TODOtree")
	  )
	 ((org-agenda-sorting-strategy
	   '(todo-state-down priority-down)
	   )
	  )
	 )
	("tb" "Business TODOs"
	 (
	  (tags-todo "+business-TODOtree")
	  (tags-todo "+business+TODOtree")
	  )
	 ((org-agenda-sorting-strategy
	   '(todo-state-down priority-down)
	   )
	  )
	 )
	("td" tags-todo "datasources")
	("tr" "Background reading"
	 (
	  (tags "reading")
	  )
	 (
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-overriding-columns-format " %3PRIORITY %50ITEM %25CATEGORY")
	  )
	 )
	("ta" "All TODOs"
	 (
	  (tags-todo "-TODOtree")
	  (tags-todo "+TODOtree")
	  )
	 ((org-agenda-sorting-strategy
	   '(todo-state-down priority-down)
	   )
	  )
	 )
	)
      )

     ;; capture templates for org-roam
     (setq

      ;; capture into org-roam
      org-roam-capture-templates
      `(

	;; default template
	("g" "generic" plain "%?"
	 :target
	 (file+head
	  "%<%Y%m%d%H%M%S>-${slug}.org"
	  ,(concat
	    "#+title: ${title}\n"
	    "#+category: ${title}\n"
	    "#+filetags: ${project-tag}"
	    )
	  )
	 :unnarrowed t
	 :jump-to-captured
	 )

	;; project-linked node
	("t" "task" plain
	 (file ,(dbargman/org-capture-get-template "task"))
	 :target
	 (file "%<%Y%m%d%H%M%S>-${slug}.org")
	 :jump-to-captured
	 )

	;; project-linked node from email
	("e" "task from email" plain
	 (file ,(dbargman/org-capture-get-template
		 "task-from-email"))
	 :target
	 (file "%<%Y%m%d%H%M%S>-${slug}.org")
	 :jump-to-captured
	 )

	;; project-linked node from email
	("b" "literate bash script using org-babel" plain
	 (file ,(dbargman/org-capture-get-template
		 "literate-bash-script"))
	 :target
	 (file+head
	  "%<%Y%m%d%H%M%S>-${slug}.org"
	  ,(concat
	    ":PROPERTIES:\n"
	    ":ROAM_REFS: "
	    "file:/mnt/projects/statosphere/org-scripts/${slug}.sh\n"
	    ":END:"
	    )
	  )
	 :jump-to-captured
	 )

	)

      ;; capture into dailies
      org-roam-dailies-capture-templates
      `(
	("s" "email with project notes" entry "\n\n* Email\n\n%?"
	 :target
	 (file+head+olp
	  "%<%Y-%m-%d>.org"
	  "#+title: %<%Y-%m-%d>\n"
	  ("Email" "Notes")
	  )
	 )

	("d" "email with a data source" entry "\n\n* Email\n\n%?"
	 :target
	 (file+head+olp
	  "%<%Y-%m-%d>.org"
	  "#+title: %<%Y-%m-%d>\n"
	  ("Email" "Data sources")
	  )
	 )

	;; progress notes
	("p" "Progress note" entry "\n\n* Progress notes\n\n%?"
	 :target
	 (file+head+olp
	  "%<%Y-%m-%d>.org"
	  "#+title: %<%Y-%m-%d>\n"
	  ("Progress notes")
	  )
	 )
	)

      )

     ;; rules for capturing emails from mu4e into org-roam
     (setq
      dbargman/email-capture-rules
      `(
	,(dbargman/capture-rules-by-subject-prefix
	  "^sttospr: ?" "PRE_MVP" t t)
	,(dbargman/capture-rules-by-subject-prefix
	  "^datasource: ?" "POST_MVP" t t)
	,(dbargman/capture-rules-by-subject-prefix
	  "^data source: ?" "POST_MVP" t t)
	)
      )

     ;; refresh org-mode buffers
     (dbargman/global-org-mode-restart)

     )
  )
 )
 )


(provide 'personalizations)
;;; personalizations.el ends here
