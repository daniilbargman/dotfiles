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

;; allow refiling between all agenda files
(setq org-refile-targets '((nil :maxlevel . 1)
			   (org-agenda-files :maxlevel . 1))
      org-refile-use-outline-path 'buffer-name
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes t)

;; add Verb to run HTTP(S) requests from Org files
(use-package verb)

;; LaTeX support
(use-package latex-preview-pane)

(provide 'org)

;;; org.el ends here
