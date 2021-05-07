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

;; helper function: get string contents of a file
(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; set groupings and filters for buffers in tab navigation
(with-eval-after-load "windows-and-tabs"

  ;; ;; set list of project tabs
  ;; (tab-bar-tabs-set
  ;;  '(
  ;;    (tab (name . "dotfiles") (explicit-name))
  ;;    (tab (name . "statosphere-root") (explicit-name))
  ;;    (tab (name . "statosphere-source") (explicit-name))
  ;;    (current-tab (name . "statosphere-frontend") (explicit-name))
  ;;    (tab (name . "statosphere-infrastructure") (explicit-name))
  ;;    (tab (name . "mu4e") (explicit-name))
  ;;    )
  ;;  )

  ;; set groupings for relevant tabs
  (setq windows-and-tabs-buffer-groups-by-name-regex
	'(("^[*]kubernetes.*[*]$" . "interactive")
	  ;; ("^task-queue[.]org[a-zA-Z/-<>]*" . "tasks")
	  ;; ("^README.*$" . "docs")
	  ("^eaf-.*$" . "widgets")
	  ("^[.]dir-locals[.]el$" . "dir-locals")
	  ;; ("^[.]bash.*$" . "dotfiles")
	  ;; ("^[a-zA-Z0-9-_.]+[.]conf[a-zA-Z/-<>]*$" . "config-files")
	  ;; ("^[a-zA-Z0-9-_.]+[.]json[a-zA-Z/-<>]*$" . "config-files")
	  ;; ("[.].*rc$" . "dotfiles")
	  ("^[*].*[*]$" . "_system")
	  ))

  ;; set groupings for relevant tabs
  (setq windows-and-tabs-buffer-groups-by-major-mode
	'((term-mode . "interactive")
	  (shell-mode . "interactive")
	  (eaf-mode . "interactive")
	  ;; (python-mode . "python")
	  ;; (emacs-lisp-mode . "emacs-config")
	  ;; (yaml-mode . "yaml")
	  ;; (sh-mode . "scripts")
	  ;; (js-mode . "web-dev")
	  ;; (svelte-mode . "web-dev")
	  ;; (html-mode . "web-dev")
	  ))

  ;; set groupings for relevant tabs
  (setq windows-and-tabs-buffer-groups-by-project-path
	'(("/home/daniilbargman" . "Project: dotfiles")
	  ("/mnt/projects/statosphere/source" . "Project: sttospr-source")
	  ("/mnt/projects/statosphere/backend" . "Project: sttospr-backend")
	  ("/mnt/projects/statosphere/services" . "Project: sttospr-services")
	  ("/mnt/projects/statosphere/endpoint-api" . "Project: sttospr-endpoints")
	  ("/mnt/projects/statosphere/frontend" . "Project: sttospr-frontend")
	  ("/mnt/projects/statosphere/infrastructure" . "Project: sttospr-infrastructure")
	  ("/mnt/projects/statosphere" . "Project: sttospr-root")
	  ;; (python-mode . "python")
	  ;; (emacs-lisp-mode . "emacs-config")
	  ;; (yaml-mode . "yaml")
	  ;; (sh-mode . "scripts")
	  ;; (js-mode . "web-dev")
	  ;; (svelte-mode . "web-dev")
	  ;; (html-mode . "web-dev")
	  ))

  ;; hide tabs for buffers that aren't frequently useful
  (setq windows-and-tabs-buffer-filter-regexp-list
	'("^[*]ovpn-mode[*]$"
	  "^[*]mount-statosphere[*]$"
	  "^[*]quelpa-.*[*]$"
	  "^[*]Messages[*]$"
	  "^[*]tramp/sudo .*[*]$"
	  "^[*]scratch[*]$"
	  "^ *[*]company-.*$"
	  "^ *[*]Treemacs-.*$"
	  "^ *[*]lsp-ui-.*$"
	  "^ *[*]mu4e-.*$"
	  "^ *[*]Org[ -].*$"
	  "^ *[*]EPC Server .*$"
	  "^ *[*]epc con .*$"
	  "^ *Download: .*$"
	  "^task-queue[.]org-archive[a-zA-Z/-<>]*"
	  "^.*[.]ovpn$"))
)

;; ;; append save values to file-local variables
;; (add-to-list 'safe-local-variable-values '((lsp-pyls-server-command
;; 	      . "/homee/daniilbargman/miniconda3/envs/statosphere/bin/pyls")))

;; mount encrypted project drive
(with-eval-after-load "bind-terminal-shell"
 (let ((bind-terminal-shell-program
	(mapconcat 'identity
		    '("/home/daniilbargman/executables/decrypt-and-mount"
		      "nvme0n1p9"
		      "projects")
		    " ")))
   (get-or-create-shell-buffer "mount-statosphere" nil nil t)
   (comint-watch-for-password-prompt
    (buffer-substring-no-properties (line-beginning-position)
				     (line-end-position)))
   (while (comint-check-proc (current-buffer))
     (comint-watch-for-password-prompt
    (buffer-substring-no-properties (line-beginning-position)
				     (line-end-position)))
     (sleep-for 1))
   ;; (comint-watch-for-password-prompt)
     ;; (comint-send-invisible "Enter passphrase for nvme0n1p9: ")
   (evil-quit)
   ))


;; start vpn client on startup, kill before exiting emacs
(with-eval-after-load "openvpn"
  (ovpn-mode-start-vpn-conf
   "/home/daniilbargman/.vpn/dbargman-server2.ovpn")
  ;; (advice-add
  ;;  'save-buffers-kill-terminal :before
  ;;   #'(lambda (&optional arg)
  ;; 	(ovpn-mode-stop-vpn-conf
  ;; 	 "/home/daniilbargman/.vpn/dbargman-server2.ovpn")
  ;; 	arg))
 )


;; set up email client
(with-eval-after-load "email"

  ;; configure email backend for mu4e
  (let (

	;; path to the maildir
	(maildir-path
	 "/mnt/projects/statosphere/Business/mail/daniil.bargman/")

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

      ;; ;; if you need offline mode, set these -- and create the queue
      ;; ;; dir with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
      ;; smtpmail-queue-mail  nil
      ;; smtpmail-queue-dir  "/home/user/Maildir/queue/cur"
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
