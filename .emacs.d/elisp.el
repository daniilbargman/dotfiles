;;; elisp.el --- ELisp configuration -*- lexical-binding: t; -*-

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

;; Configuration for ELisp files

;;; Code:

;;; set tab width to 2
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (defvar evil-shift-width) (setq evil-shift-width 2)))

; auto-indents in increments of 2 as well
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24))

(provide 'elisp)
;;; elisp.el ends here
