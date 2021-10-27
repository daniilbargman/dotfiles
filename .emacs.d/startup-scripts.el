;;; startup-scripts.el --- startup scripts -*- lexical-binding: t -*-

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


;; mount encrypted project drive
(start-process "mount-projects" "*startup-scripts--mount-projects*"
	       "/home/daniilbargman/executables/decrypt-and-mount"
	       "nvme0n1p9"
	       "projects")
(with-current-buffer "*startup-scripts--mount-projects*"
  (comint-watch-for-password-prompt
      (buffer-substring-no-properties (line-beginning-position)
				      (line-end-position)))
    (while (comint-check-proc (current-buffer))
      (comint-watch-for-password-prompt
      (buffer-substring-no-properties (line-beginning-position)
				      (line-end-position)))
      (sleep-for 1))
    )



;; start vpn client on startup
(with-eval-after-load "openvpn"
  (ovpn-mode-start-vpn-conf
   "/home/daniilbargman/.vpn/dbargman-server2.ovpn")
 )


;; set up email client
(with-eval-after-load "email"

  ;; helper function: get string contents of a file
  (defun get-string-from-file (filePath)
    "Return FILEPATH's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  ;; configure email backend for mu4e
  (let (

	;; path to the maildir
	(maildir-base
	 "/mnt/projects/statosphere/Business/mail/")

	)

    ;; set database directories for getmail and mu4e
    (setq
      maildir-path (concat maildir-base "daniil.bargman/")
      mu4e-mu-home (concat maildir-base "muhome/")
    )

    ;; command for retrieving emails
    (setq
     mu4e-get-mail-command
     (concat "getmail -v --getmaildir=" maildir-path)

     ;; update every minute
     mu4e-update-interval 60
     )

    ;; general smtpmail settings
    (setq
     smtpmail-smtp-service 465
     smtpmail-stream-type 'ssl
     )

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setq mu4e-compose-reply-to-address "daniil.bargman@statosphere.com"
	  user-mail-address "daniil.bargman@statosphere.com"
	  user-full-name  "Daniil Bargman")

    ;; smtp mail setting
    (setq
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "mail.statosphere.com"
      smtpmail-smtp-server "mail.statosphere.com"
      smtpmail-local-domain "statosphere.com"

      )

    ;; org-msg customizations, including signature
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi *%s*,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-default-alternatives '(text html)
	org-msg-convert-citation t
	org-msg-signature
	  (get-string-from-file
	   (concat maildir-path "email-signature.html"))))

  ;; the following only makes sense once org mode is loaded
  (with-eval-after-load "org"

    ;; org capture templates for project notes
    (defvar org-capture-templates (list))
    (setq org-capture-templates
	  (append org-capture-templates
	  `(("s" "process emails as statosphere tasks")
	    ("st"
	     "capture email as a task"
	     entry
	     (file "/mnt/projects/statosphere/task-queue.org")
	     ,(concat
	      "* %(string-trim-left \"%:subject\" \"sttospr:[ ]*\")\n"
	      "- email date: %:date-timestamp-inactive\n"
	      "- [[%:link][email link (C-c C-o)]]")
	     :empty-lines-before 1
	     :immediate-finish
	     :kill-buffer
	     )
	    ("sd"
	     "capture email as a data source idea"
	     entry
	     (file "/mnt/projects/statosphere/datasources.org")
	     ,(concat
	      "* %(string-trim-left \"%:subject\" \"datasource:[ ]*\")\n"
	      "- email date: %:date-timestamp-inactive\n"
	      "- [[%:link][email link (C-c C-o)]]")
	     :empty-lines-before 1
	     :immediate-finish
	     :kill-buffer
	     )
	    ("d" "process emails as dotfile tasks")
	    ("dt"
	     "capture email as a task"
	     entry
	     (file+headline "~/notes-to-self.org" "Unsorted")
	     ,(concat
	      "* %(string-trim-left \"%:subject\" \"nts:[ ]*\")\n"
	      "- email date: %:date-timestamp-inactive\n"
	      "- [[%:link][email link (C-c C-o)]]")
	     :empty-lines-before 1
	     :immediate-finish
	     :kill-buffer
	     )
	    )
	  )
      )

    ;; command for moving emails with special titles to unsorted
    ;; sections in dedicated org files
    (defun move-email-notes-to-orgfile ()
	"Move email notes from myself to unsorted org file."
	(interactive)
	(link-emails-to-org-file
	 "from:daniil.bargman and flag:unread"
	 '(((lambda (msg)
	      (string-match
	       "^sttospr:.+"
	       (mu4e-message-field msg :subject)))
	    . "st")
	   ((lambda (msg)
	      (string-match
	       "^datasource:.+"
	       (mu4e-message-field msg :subject)))
	    . "sd")
	   ((lambda (msg)
	      (string-match
	       "^nts:.+"
	       (mu4e-message-field msg :subject)))
	    . "dt"))
	 t t)
	)
      (define-key mu4e-main-mode-map
	(kbd "C-c c") 'move-email-notes-to-orgfile)
      (define-key 'mu4e-headers-mode-map
	(kbd "C-c c") 'move-email-notes-to-orgfile)
      )
  )


(provide 'startup-scripts)
;;; startup-scripts.el ends here
