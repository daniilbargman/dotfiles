;;; bash.el --- Bash configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Daniil Bargman
;; Keywords: configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration for Bash files

;;; Code:

;; 79-column files
(add-hook 'sh-mode-hook (lambda () (setq-local fill-column 79)))

;;; set tab width to 2
(add-hook 'sh-mode-hook
	  (lambda ()
	    (defvar evil-shift-width) (setq-local evil-shift-width 2)))

;; auto-indents in increments of 2 as well
(add-hook 'sh-mode-hook (lambda () (setq-local tab-stop-list '(2 4))))

;; use the basic completion at point function
(add-hook 'sh-mode-hook
	  (lambda () (setq-local completion-at-point-functions
				 '(tags-completion-at-point-function t))))

(provide 'bash)
;;; bash.el ends here
