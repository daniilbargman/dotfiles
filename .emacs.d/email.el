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

(add-to-list 'load-path "~/.emacs.d/mu/mu4e")
(require 'mu4e)

;; point to mu binary
(setq mu4e-mu-binary "/usr/local/bin/mu")

;; use mu4e for e-mail in emacs
(setq mail-user-agent (mu4e-user-agent))

;; the next are relative to the root maildir
;; (see `mu info`).
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
(setq mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash"
      mu4e-refile-folder  "/archive")

;; NOTE: may wish to set this later in keybindings.el
;; ;; the maildirs you use frequently; access them with 'j' ('jump')
;; (setq   mu4e-maildir-shortcuts
;;     '((:maildir "/archive" :key ?a)
;;       (:maildir "/inbox"   :key ?i)
;;       (:maildir "/work"    :key ?w)
;;       (:maildir "/sent"    :key ?s)))

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)    ;; alternatively, use :human-date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil))) ;; alternatively, use :thread-subject

;; don't show update warnings
(setq mu4e-index-update-error-warning nil)

;; don't split window when opening messages
(setq mu4e-split-view nil)

;; use fancy characters in mu4e
(setq mu4e-use-fancy-chars t)

;; prefer HTML body over plaintext body, when available
(setq mu4e-view-prefer-html t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


;;; integrate with org-msg
(use-package org-msg
  :requires (emacs-htmlize))

;; enable org-msg-mode and configure for mu4e
(org-msg-mode)
(org-msg-mode-mu4e)

;; function for refiling specific incoming email directly to a
;; dedicated Org file for a project
(defun link-emails-to-org-file (email-query capture-rules &optional mark-read do-refile refile-location)
  "Capture all emails matching EMAIL-QUERY using CAPTURE-RULES.

CAPTURE-RULES must be an alist of the form:

  '((<message-test-function> . <capture-template>) ...)

Each message test function must accept the message object from the
headers view as the only argument.  Messages for which the test function
returns a non-nil value will be captured using the <capture-template>.

If MARK-READ is non-nil, messages for which at least one test function
has returned non-nil, will be marked as read.

If DO-REFILE is non-nil, messages for which at least one test function
has returned non-nil, will be refiled - either to \"/archive\" (default)
or to REFILE-LOCATION if one is set."

  ;; for each message, apply function
  (defun move-headers-to-orgfile ()
    (dolist (rule capture-rules)
      (let ((testfun (car rule))
	    (use-template (cdr rule)))
	(mu4e-headers-for-each
	  (lambda (msg)
	    "Link message to a file."
	    ;; capture into templates
	    (when (funcall testfun msg)
	      (org-capture nil use-template))))
	;; apply and execute marks as needed
	(mu4e-headers-mark-for-each-if
	  (cons 'read nil)
	  (lambda (msg param) (funcall testfun msg)))
	(when (> (mu4e-mark-marks-num) 0) (mu4e-mark-execute-all t))
	(mu4e-headers-mark-for-each-if
	  (cons 'refile (or refile-location "/archive"))
	  (lambda (msg param) (funcall testfun msg)))
	(when (> (mu4e-mark-marks-num) 0) (mu4e-mark-execute-all t))))
    ;; remove this function from hooks
    (remove-hook 'mu4e-headers-found-hook 'move-headers-to-orgfile))

    ;; add hook and fire email query
  (add-hook 'mu4e-headers-found-hook 'move-headers-to-orgfile)
  (save-excursion
    (mu4e-headers-search email-query))

    )


;; NOTE: rest is deferred to startup-scripts.el ;;

(provide 'email)
;;; email.el ends here
