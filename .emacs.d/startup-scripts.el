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


;; start vpn client
(with-eval-after-load "openvpn"
  (ovpn-mode-start-vpn-conf
  "/home/daniilbargman/.vpn/dbargman-server2.ovpn"))


;; set groupings and filters for buffers in tab navigation
(with-eval-after-load "windows-and-tabs"

  ;; set groupings for relevant tabs
  (setq windows-and-tabs-buffer-groups-by-name-regex
	'(("^[*]terminal[*]$" . "interactive")
	  ("^[*].*shell*[*]$" . "interactive")
	  ("^[*]kubernetes.*[*]$" . "interactive")
	  ("^action-log.org$" . "meta")
	  ("^TODO.*$" . "meta")
	  ("^README.*$" . "meta")
	  ("^[.]dir-locals[.]el$" . "meta")
	  ("^.*[.]yaml$" . "manifests")
	  ("^.*[.]el$" . "dotfiles")
	  (".*[.]bash.*$" . "dotfiles")
	  (".*[.]conf$" . "dotfiles")
	  (".*[.].*rc$" . "dotfiles")
	  ("^.*test.*$" . "testing")
	  ("^[a-z-]+$" . "source")
	  ("^.*[.]py$" . "source")
	  ("^[*].*[*]$" . "_system")))

  ;; hide tabs for buffers that aren't frequently useful
  (setq windows-and-tabs-buffer-filter-regexp-list
	'("^[*]ovpn-mode[*]$"
	  "^[*]mount-statosphere[*]$"
	  "^[*]quelpa-.*[*]$"
	  "^[*]Messages[*]$"
	  "^[*]tramp/sudo .*[*]$"
	  "^[*]scratch[*]$"
	  "^ *[*]company-.*[*]$"
	  "^.*[.]ovpn$"))
)


(provide 'startup-scripts)
;;; startup-scripts.el ends here
