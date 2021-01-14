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
(setq mu4e-mu-binary "~/.emacs.d/mu/mu/mu")

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

;; NOTE: rest is deferred to startup-scripts.el ;;

(provide 'email)
;;; email.el ends here
